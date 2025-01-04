(select-module test.unit)
(use number-table)

(define-assertion (assert-number-table num-of-row max-num table)
  (assert-equal num-of-row (row-number-of table)
                (lambda (actual)
                  (format " expected:<~s> #rows\n  but was:<~s>"
                          num-of-row actual)))
  (assert-equal `(,@(make-numbers 1 max-num) #f)
                (sort-number-table table)))

(define-assertion (assert-available-way west north east south table)
  (receive (w n e s)
      (available-ways table)
    (for-each (lambda (expected actual way)
                (assert-equal expected actual
                              (make-message-handler
                               expected
                               :after-expected #`" for ,way")))
              (list west north east south)
              (list w n e s)
              '("west" "north" "east" "south"))))

(define-assertion (assert-empty-cell-index x y table)
  (receive (act-x act-y)
      (empty-cell-index table)
    (assert-equal x act-x (make-message-handler x :after-expected " for x"))
    (assert-equal y act-y (make-message-handler y :after-expected " for y"))))

(provide "test/common")