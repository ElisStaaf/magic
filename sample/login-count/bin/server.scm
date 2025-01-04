#!/usr/bin/env gosh

(use gauche.interactive)
(use magic.server)
(use login-count-servlet)

(define *login-count-port* 5969)
(define *login-count-mount-point* "/login-count")

(define (main args)
  (let ((servlet (make-login-count-servlet))
        (server (make-magic-server
                 #`"dsmp://:,|*login-count-port*|")))
    (add-mount-point! server *login-count-mount-point* servlet)
    (magic-server-start! server)
    (magic-server-join! server)))
