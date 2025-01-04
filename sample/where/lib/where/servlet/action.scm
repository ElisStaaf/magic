(define-module where.servlet.action
  (use magic.action)
  (export do-left do-center do-right))
(select-module where.servlet.action)

(define (move! where)
  (set-value! 'prev (get-value 'where "??"))
  (set-value! 'where where))

(define (do-default) 'show)

(define (do-left)
  (move! "left") 'show)

(define (do-center)
  (move! "center") 'show)

(define (do-right)
  (move! "right") 'show)

(provide "where.servlet.action")
