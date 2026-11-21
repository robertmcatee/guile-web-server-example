#!/usr/bin/guile -s
!#
;;; cenzontle-server.scm — production server for www.cenzontle.us.
;;;
;;; One Guile process, two listeners:
;;;   :443  HTTPS via GnuTLS. ALL gnutls work — credentials,
;;;         accept, handshake, serving — happens on ONE dedicated
;;;         worker thread, sequentially. Requests are parsed with
;;;         (web request), responses written with (web response);
;;;         the docroot is preloaded into memory at startup.
;;;   :80   Served by the main thread: ACME http-01 challenges read
;;;         fresh from disk; everything else 301-redirects to https.
;;;
;;; Why a single TLS thread: measured on the Pi on 2026-10-23, the
;;; bullseye guile-gnutls (3.7.1) bindings fail TLS handshakes with
;;; "Public key signing has failed" when sessions are created on
;;; freshly spawned connection threads — the same code is 100%
;;; reliable when every gnutls call happens on one thread (and the
;;; newer stack in the sandbox, Guile 3.0 + bindings 3.7.9, does not
;;; care). Sequential serving costs nothing here: the page is 70
;;; bytes and traffic is a trickle; the TLS handshake (~0.5 s of
;;; ARMv6 software crypto) dominates every request anyway.
;;;
;;; Two further design points, also measured on 2026-10-23:
;;;   - Exactly one request per connection. Looping for a keep-alive
;;;     follow-up parks the thread in a record read that most
;;;     clients never complete, and Guile 2.2 offers no way to
;;;     interrupt another thread's port read.
;;;   - Every wait is gated with select() and a deadline (10 s), so
;;;     a client that connects and then stalls — before the
;;;     handshake or before its request — costs the worker seconds,
;;;     not forever.
;;;
;;; Env: CENZONTLE_DOCROOT  (default /var/www/www.cenzontle.us)
;;;      CENZONTLE_CERT     (default /etc/letsencrypt/live/www.cenzontle.us/fullchain.pem)
;;;      CENZONTLE_KEY      (default /etc/letsencrypt/live/www.cenzontle.us/privkey.pem)
;;;      CENZONTLE_PORT_HTTPS / CENZONTLE_PORT_HTTP (defaults 443/80)

(use-modules (gnutls)
             (web request)
             (web response)
             (web uri)
             (ice-9 binary-ports)
             (ice-9 textual-ports)
             (ice-9 threads)
             (ice-9 ftw)
             (rnrs bytevectors))

(define docroot
  (or (getenv "CENZONTLE_DOCROOT") "/var/www/www.cenzontle.us"))
(define cert-file
  (or (getenv "CENZONTLE_CERT")
      "/etc/letsencrypt/live/www.cenzontle.us/fullchain.pem"))
(define key-file
  (or (getenv "CENZONTLE_KEY")
      "/etc/letsencrypt/live/www.cenzontle.us/privkey.pem"))
(define port-https
  (string->number (or (getenv "CENZONTLE_PORT_HTTPS") "443")))
(define port-http
  (string->number (or (getenv "CENZONTLE_PORT_HTTP") "80")))

(define (read-file-bytes path)
  (call-with-input-file path
    (lambda (port) (get-bytevector-all port))
    #:binary #t))

;;; ---- page cache: URL path -> body bytes, built once at startup --
(define cache (make-hash-table))
(let walk ((dir docroot) (prefix ""))
  (for-each
   (lambda (name)
     (let ((full (string-append dir "/" name))
           (url  (string-append prefix "/" name)))
       (cond
        ((eq? (stat:type (stat full)) 'directory)
         (let ((index (string-append full "/index.html")))
           (when (file-exists? index)
             (hash-set! cache (string-append url "/")
                        (read-file-bytes index))))
         (walk full url))
        ((string-suffix? ".html" name)
         (hash-set! cache url (read-file-bytes full))))))
   (scandir dir (lambda (n) (not (member n '("." "..")))))))
(hash-set! cache "/"
           (read-file-bytes (string-append docroot "/index.html")))

;;; ---- plumbing -------------------------------------------------------
(define (listener port)
  (let ((sock (socket PF_INET SOCK_STREAM 0)))
    (setsockopt sock SOL_SOCKET SO_REUSEADDR 1)
    (bind sock AF_INET INADDR_ANY port)
    (listen sock 128)
    sock))

;; #t if SOCK has bytes to read within SECS seconds.
(define (wait-readable sock secs)
  (catch #t
    (lambda ()
      (pair? (car (select (list sock) '() '() secs 0))))
    (lambda _ #f)))

(define (log-error where key args)
  (format (current-error-port) "~a error: ~s ~s\n" where key args)
  (force-output (current-error-port)))

;;; ---- HTTPS: everything gnutls on this one worker thread -------------
(define (handle-https client credentials)
  (let ((session (make-session connection-end/server)))
    (set-session-default-priority! session)
    (set-session-credentials! session credentials)
    (set-session-transport-port! session client)
    ;; Gate the handshake: a client that connects and says nothing
    ;; gets 10 seconds, not the whole server.
    (unless (wait-readable client 10)
      (throw 'stalled "no ClientHello within 10s"))
    (handshake session)
    (let ((tls (session-record-port session)))
      ;; Gate the request the same way.
      (when (wait-readable client 10)
        (let ((req (read-request tls)))
          (when req
            (let ((len (request-content-length req)))
              (when (and len (> len 0))
                (get-bytevector-n tls len))) ; drain any body
            (let* ((path (uri-path (request-uri req)))
                   (body (and (not (string-contains path ".."))
                              (hash-ref cache path #f))))
              (if body
                  (let ((resp (build-response
                               #:headers
                               `((content-length
                                  . ,(bytevector-length body))))))
                    (write-response resp tls)
                    (put-bytevector tls body))
                  (let ((resp (build-response
                               #:code 404
                               #:headers '((content-length . 9)))))
                    (write-response resp tls)
                    (put-bytevector tls #u8(110 111 116 32 104 101
                                            114 101 10))))
              (force-output tls)))))
      ;; close-request/wr: send our close_notify but never wait for
      ;; the peer's - waiting (rdwr) can park forever.
      (catch #t (lambda () (bye session close-request/wr))
             (lambda _ #f)))))

(define (tls-worker https-sock)
  ;; The credentials are created HERE, on the worker thread: on the
  ;; Guile 2.2 / gnutls 3.7.1 stack, gnutls state is only reliable
  ;; when it never crosses threads.
  (let ((credentials (make-certificate-credentials)))
    (set-certificate-credentials-x509-key-files!
     credentials cert-file key-file x509-certificate-format/pem)
    (let loop ()
      (catch #t
        (lambda ()
          (let ((client (car (accept https-sock))))
            (catch #t
              (lambda () (handle-https client credentials))
              (lambda (key . args) (log-error "tls" key args)))
            (catch #t (lambda () (close-port client))
                   (lambda _ #f))))
        (lambda (key . args) (log-error "tls-accept" key args)))
      (loop))))

;;; ---- HTTP: ACME + redirect (main thread) ------------------------------
(define acme-prefix "/.well-known/acme-challenge/")

(define (handle-http client)
  (let ((line (get-line client)))
    (when (string? line)
      (let drain ()
        (let ((h (get-line client)))
          (when (and (string? h) (not (string-null? h))
                     (not (string=? h "\r")))
            (drain))))
      (let* ((parts (string-split line #\space))
             (path  (and (>= (length parts) 2) (cadr parts))))
        (cond
         ((and path (string-prefix? acme-prefix path))
          (let* ((name (substring path (string-length acme-prefix)))
                 (file (string-append docroot acme-prefix name)))
            (if (and (not (string-contains name "/"))
                     (not (string-contains name ".."))
                     (file-exists? file))
                (begin
                  (format client
                          "HTTP/1.1 200 OK\r\nConnection: close\r\n\r\n")
                  (put-bytevector client (read-file-bytes file)))
                (format client
                        "HTTP/1.1 404 Not Found\r\nConnection: close\r\n\r\n"))))
         (path
          (format client
                  "HTTP/1.1 301 Moved Permanently\r\nLocation: https://www.cenzontle.us~a\r\nConnection: close\r\n\r\n"
                  path))
         (else
          (format client
                  "HTTP/1.1 400 Bad Request\r\nConnection: close\r\n\r\n")))))
    (force-output client)))

(define (http-loop http-sock)
  (let loop ()
    (catch #t
      (lambda ()
        (let ((client (car (accept http-sock))))
          (catch #t
            (lambda ()
              (when (wait-readable client 10)
                (handle-http client)))
            (lambda (key . args) (log-error "http" key args)))
          (catch #t (lambda () (close-port client)) (lambda _ #f))))
      (lambda (key . args) (log-error "http-accept" key args)))
    (loop)))

;;; ---- main --------------------------------------------------------------
(let ((https-sock (listener port-https))
      (http-sock  (listener port-http)))
  (call-with-new-thread (lambda () (tls-worker https-sock)))
  (http-loop http-sock))
