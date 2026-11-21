#!/usr/bin/guile -s
!#
;;; cenzontle.scm — pure-Guile trial server for the www.cenzontle.us site.
;;;
;;; Serves the docroot's static files (the 70-byte 1kb.club page is
;;; index.html) using only Guile's standard (web server). Trial build:
;;; binds port 8080 on all interfaces; nginx keeps 80/443 untouched.
;;; Run:  guile -s cenzontle.scm     (or ./cenzontle.scm)
;;;
;;; Deliberately minimal: one request at a time, no TLS, no logging
;;; beyond what the (web server) REPL of errors prints to stderr.

(use-modules (web server)
             (web request)
             (web response)
             (web uri)
             (ice-9 binary-ports))

(define docroot
  (or (getenv "CENZONTLE_DOCROOT") "/var/www/www.cenzontle.us"))

(define (read-file-bytes path)
  (call-with-input-file path
    (lambda (port) (get-bytevector-all port))
    #:binary #t))

;; No Content-Type is sent, deliberately — the same call the production
;; nginx makes with more_clear_headers. Browsers sniff these simple
;; files fine, and the ranked page's header block stays minimal.
(define (serve-file path)
  (values (build-response)
          (read-file-bytes path)))

(define (handler request body)
  (let ((path (uri-path (request-uri request))))
    (cond
      ;; No directory escapes: the docroot is the whole world.
      ((string-contains path "..")
       (values (build-response #:code 404) "not here\n"))
      (else
       (let ((target (string-append docroot path)))
         (cond
           ;; Directory (including "/") -> its index.html.
           ((and (file-exists? target)
                 (eq? (stat:type (stat target)) 'directory)
                 (file-exists? (string-append target "/index.html")))
            (serve-file (string-append target "/index.html")))
           ;; A regular file under the docroot.
           ((and (file-exists? target)
                 (eq? (stat:type (stat target)) 'regular))
            (serve-file target))
           (else
            (values (build-response #:code 404) "not here\n"))))))))

(run-server handler 'http '(#:port 8080 #:addr 0))
