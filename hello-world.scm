(use-modules (web server))
(use-modules (web request)
             (web response)
             (web uri)
	     (sxml simple))

(define (templatize title body)
  `(html (head (title ,title))
         (body ,@body)))

(define* (respond #:optional body #:key
		  (status 200)
		  (title "Hello hello!")
		  (doctype "<!DOCTYPE html>\n")
		  (content-type-params '((charset . "utf-8")))
		  (content-type 'text/html)
		  (extra-headers '())
		  (sxml (and body (templatize title body))))
  (values (build-response
	   #:code status
	   #:headers `((content-type
			. (,content-type ,@content-type-params))
		       ,@extra-headers))
	  (lambda (port)
	    (if sxml
		(begin
		  (if doctype (display doctype port))
		  (sxml->xml sxml port))))))

(define (request-path-components request)
  (split-and-decode-uri-path (uri-path (request-uri request))))

(define (not-found request)
  (values (build-response #:code 404)
	  (string-append "Resource not found: "
			 (uri->string (request-uri request)))))

(define (site request body)
  (cond
   ((null? (request-path-components request))
    (respond
     `((h1 "hello world!!")
       (table
	(tr (th "header") (th "value"))
	,@(map (lambda (pair)
		 `(tr (td (tt ,(with-output-to-string
				 (lambda () (display (car pair))))))
		      (td (tt ,(with-output-to-string
				 (lambda ()
				   (write (cdr pair))))))))
	       (request-headers request))))))
   ((equal? (request-path-components request) '("hacker"))
    (respond
     `((h1 "hello hacker!!")
       (table
	(tr (th "header") (th "value"))
	,@(map (lambda (pair)
		 `(tr (td (tt ,(with-output-to-string
				 (lambda () (display (car pair))))))
		      (td (tt ,(with-output-to-string
				 (lambda ()
				   (write (cdr pair))))))))
	       (request-headers request))))))
   (else (not-found request))))

(run-server site)
