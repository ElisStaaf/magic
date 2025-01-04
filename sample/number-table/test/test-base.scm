#!/usr/bin/enb gosh

(use test.unit)
(require "test/common")
(use number-table)

(define-test-case "base test"
  ("make-numbers test"
   (assert-equal '(1) (make-numbers 1 1))
   (assert-equal '(0 1 2) (make-numbers 0 2))
   (assert-equal '() (make-numbers 3 2))
   )
  ("sort-numbers test"
   (assert-equal '(1 2 3) (sort-numbers '(2 3 1)))
   (assert-equal '(1 2 3 #f) (sort-numbers '(2 #f 3 1)))
   )
  ("flatten test"
   (assert-equal '(1) (flatten '(1)))
   (assert-equal '(1 2 3) (flatten '((1) (2 (3)))))
   )
  ("row-number-of test"
   (assert-equal 0 (row-number-of '()))
   (assert-equal 1 (row-number-of '(1)))
   (assert-equal 2 (row-number-of '(1 2)))
   (assert-equal 1 (row-number-of '((1 2)))))
  ("column-number-of test"
   (assert-equal 0 (column-number-of '()))
   (assert-equal 1 (column-number-of '(1)))
   (assert-equal 1 (column-number-of '(1 2)))
   (assert-equal 2 (column-number-of '((1 2)))))
  ("make-number-table test"
   (assert-number-table 2 3 (make-number-table 2)))
  ("find-emtpy-cell-index test"
   (assert-empty-cell-index 0 0
                            '((#f 1)
                              (2 3)))
   (assert-empty-cell-index 0 1
                            '((1 2)
                              (#f 3)))
   (assert-empty-cell-index 2 1
                            '((1 2 3)
                              (4 5 #f)
                              (6 7 8)))
   (assert-empty-cell-index #f #f
                            '((1))))
  ("available-way test"
   (assert-available-way #f #f #f #f
                         '((1)))
   (assert-available-way #t #t #t #t
                         '((1 2 3)
                           (4 #f 5)
                           (6 7 8)))
   (assert-available-way #t #t #f #f
                         '((1 2 3)
                           (4 5 6)
                           (7 8 #f)))
   (assert-available-way #f #f #t #t
                         '((#f 1 2)
                           (3 4 5)
                           (6 7 8)))
   (assert-available-way #f #t #t #f
                         '((1 2)
                           (#f 3))))
  ("get-cell test"
   (assert-equal 1 (get-cell '((1 2)
                               (3 4))
                             0 0))
   (assert-equal 2 (get-cell '((1 2)
                               (3 4))
                             1 0))
   (assert-equal 3 (get-cell '((1 2)
                               (3 4))
                             0 1))
   (assert-equal 4 (get-cell '((1 2)
                               (3 4))
                             1 1)))
  ("set-cell! test"
   (assert-equal '((1 2)
                   (3 4))
                 (set-cell! '((#f 2)
                              (3 4))
                            0 0
                            1))
   (assert-equal '((1 2)
                   (3 4))
                 (set-cell! '((1 #f)
                              (3 4))
                            1 0
                            2))
   (assert-equal '((1 2)
                   (3 4))
                 (set-cell! '((1 2)
                              (#f 4))
                            0 1
                            3))
   (assert-equal '((1 2)
                   (3 4))
                 (set-cell! '((1 2)
                              (3 #f))
                            1 1
                            4)))
  ("swap-cell! test"
   (assert-equal '((1 2)
                   (3 #f))
                 (swap-cell! '((1 2)
                               (#f 3))
                             0 1
                             1 1))
   (assert-equal '((1 2)
                   (3 #f))
                 (swap-cell! '((1 #f)
                               (3 2))
                             1 0
                             1 1)))
  ("move! test"
   (assert-equal '((1 2)
                   (3 #f))
                 (move! '((1 2)
                          (#f 3))
                        'east))
   (assert-equal '((1 2)
                   (#f 3))
                 (move! '((1 2)
                          (3 #f))
                        'west))
   (assert-equal '((1 #f)
                   (3 2))
                 (move! '((1 2)
                          (3 #f))
                        'north))
   (assert-equal '((1 2)
                   (3 #f))
                 (move! '((1 #f)
                          (3 2))
                        'south)))
  ("clear? test"
   (assert-true (clear? '((1 2)
                          (3 #f))))
   (assert-false (clear? '((1 #f)
                           (3 2)))))
  ("shuffle-table test"
   (let* ((row-size 4)
          (table (make-number-table row-size)))
     (assert-true (clear? table))
     (shuffle-table! table)
     (assert-false (clear? table))
     (assert-equal `(,@(make-numbers 1 (- (* row-size row-size) 1))
                     #f)
                   (sort-numbers (flatten table)))))
  )

