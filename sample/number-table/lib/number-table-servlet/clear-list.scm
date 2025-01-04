(define-module number-table-servlet.clear-list
  (export make-clear-list
          clear-list-user
          clear-list-score
          clear-list-time)
(select-module number-table-servlet.clear-list)

(define (make-clear-list user score)
  (list user score (sys-time)))

(define (clear-list-user cl) (car cl))

(define (clear-list-score cl) (cadr cl)))

(define (clear-list-time cl)
  (sys-asctime (sys-localtime (caddr cl))))

(provide "number-table-servlet/clear-list")