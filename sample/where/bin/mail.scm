#!/usr/bin/env gosh

(use magic.client.mail)

(define *where-server* "localhost")
(define *where-port* 5979)
(define *where-mount-point* "/where")

(define (main args)
  (magic-mail-main #`"dsmp://,|*where-server*|:,|*where-port*|"
                     *where-mount-point*
                     (current-input-port)
                     :http-base (get-optional (cdr args) #f)))
