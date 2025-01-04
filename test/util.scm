(use test.unit)

(define-macro (assert-each-list-elem prepare-proc . keywords)
  (let-keywords* keywords ((assert-proc assert-equal)
                           (lists '()))
    `(assert-each ,assert-proc
                  ,lists
                  :prepare (lambda (lst)
                             (apply ,prepare-proc lst)))))
