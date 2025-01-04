(define-module magic.client.phttpd-servlet
  (extend magic.client.cgi)
  (export make-magic-phttpd-servlet)
  )
(select-module magic.client.phttpd-servlet)

(define (make-magic-phttpd-servlet client mount-point . servlet-args)
  (lambda (args)
    (apply magic-cgi-main client mount-point servlet-args)))

(provide "magic/client/phttpd-servlet")
