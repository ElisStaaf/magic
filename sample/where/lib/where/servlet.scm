(define-module where.servlet
  (use magic.servlet)
  (export make-where-servlet))
(select-module where.servlet)

(define (make-where-servlet)
  (make <magic-servlet>
    :servlet-module-name 'where.servlet))

(provide "where.servlet")
