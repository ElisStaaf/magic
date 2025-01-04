(define-module magic.client
  (extend magic.common)
  (use dsm.client)
  (export connect-server)
  )
(select-module magic.client)

(define (connect-server . args)
  (apply (with-module dsm.client connect-server)
         `(,@args
           :eof-handler ,(lambda args #f))))

(provide "magic/client")
