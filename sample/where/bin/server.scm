#!/usr/bin/env gosh

(use gauche.interactive)
(use magic.server)
(use where.servlet)

(define *where-port* 5979)
(define *where-mount-point* "/where")

(define (main args)
  (let ((servlet (make-where-servlet))
        (server (make-magic-server #`"dsmp://:,|*where-port*|")))
    (add-mount-point! server *where-mount-point* servlet)
    (magic-server-start! server)
    (magic-server-join! server)))
