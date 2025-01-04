#!/usr/bin/env gosh

(use gauche.interactive)
(use magic.server)
(use number-table-servlet)

(define *number-table-port* 5969)
(define *number-table-mount-point* "/number-table")

(define (main args)
  (let ((servlet (make-number-table-servlet))
        (server (make-magic-server #`"dsmp://:,|*number-table-port*|")))
    (add-mount-point! server *number-table-mount-point* servlet)
    (magic-server-start! server)
    (magic-server-join! server)))
