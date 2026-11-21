#!/usr/bin/guile -s
!#
;;; cenzontle-fibers.scm — Fibers variant of the cenzontle.scm trial
;;; server. Same handler, same docroot, same deliberately minimal
;;; headers (no Content-Type); the difference is the engine: the
;;; (fibers web server) runs a fiber per connection on an epoll
;;; scheduler, so one stalled client cannot park the site the way
;;; the sequential (web server) can.
;;;
;;; Prototype: binds port 8083. Run: guile -s cenzontle-fibers.scm

(use-modules (fibers web server)
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

;; No Content-Type is sent, deliberately — same call as production
;; nginx (more_clear_headers) and as the stdlib trial build.
(define (serve-file path)
  (values (build-response)
          (read-file-bytes path)))

(define (handler request body)
  (let ((path (uri-path (request-uri request))))
    (cond
      ((string-contains path "..")
       (values (build-response #:code 404) "not here\n"))
      (else
       (let ((target (string-append docroot path)))
         (cond
           ((and (file-exists? target)
                 (eq? (stat:type (stat target)) 'directory)
                 (file-exists? (string-append target "/index.html")))
            (serve-file (string-append target "/index.html")))
           ((and (file-exists? target)
                 (eq? (stat:type (stat target)) 'regular))
            (serve-file target))
           (else
            (values (build-response #:code 404) "not here\n"))))))))

(run-server handler #:port 8083)
