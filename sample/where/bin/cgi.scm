#!/usr/bin/env gosh

(use magic.client.cgi)

(define *where-server* "localhost")
(define *where-port* 5979)
(define *where-mount-point* "/where")

(define (main args)
  (magic-cgi-main #`"dsmp://,|*where-server*|:,|*where-port*|"
                    *where-mount-point*
                    :debug #t))
