#!/usr/bin/env gosh

(use magic.client.cgi)

(define *login-count-server* "localhost")
(define *login-count-port* 5969)
(define *login-count-mount-point* "/login-count")

(define (main args)
  (magic-cgi-main #`"dsmp://,|*login-count-server*|:,|*login-count-port*|"
                    *login-count-mount-point*
                    :debug #t))
