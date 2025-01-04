(define-module number-table
  (use util.list)
  (use srfi-27)
  (use gauche.sequence)
  (export
   make-numbers sort-numbers flatten
   sort-number-table available-ways empty-cell-index
   make-number-table row-number-of column-number-of
   move! swap-cell! get-cell set-cell!
   can-move? clear? shuffle-table!))
(select-module number-table)

(random-source-randomize! default-random-source)

(define (make-numbers init max)
  (cond ((< init max)
         (cons init (make-numbers (+ 1 init) max)))
        ((= init max)
         (list init))
        (else
         '())))

(define (sort-numbers lst)
  (sort lst (lambda (x y)
              (cond ((not x) #f)
                    ((not y) #t)
                    (else (< x y))))))

(define (sort-number-table table)
  (sort-numbers (flatten table)))

(define (flatten tree)
  (cond ((null? tree) '())
        ((pair? (car tree))
         (append (flatten (car tree))
                 (flatten (cdr tree))))
        (else
         (cons (car tree)
               (flatten (cdr tree))))))

(define (make-number-table num-of-row)
  (slices `(,@(make-numbers 1 (- (* num-of-row num-of-row) 1))
            #f)
          num-of-row))

(define-method row-number-of (table)
  (length table))

(define-method column-number-of (table)
  (cond ((null? table) 0)
        ((pair? (car table)) (length (car table)))
        (else 1)))

(define (empty-cell-index table)
  (apply values
         (call/cc
          (lambda (break)
            (for-each-with-index
             (lambda (row-index row)
               (let ((cell-index (find-index (cut eq? #f <>)
                                             row)))
                 (if cell-index
                     (break (list cell-index row-index)))))
             table)
            ;; not found
            (list #f #f)))))
                                
(define (available-ways table)
  (receive (x y)
      (empty-cell-index table)
    (if (and x y)
        (values (> x 0) ; west
                (> y 0) ; north
                (< (+ 1 x) (column-number-of table)) ; east
                (< (+ 1 y) (row-number-of table))) ; south
        (values #f #f #f #f))))

(define (move! table way)
  (receive (x y)
      (empty-cell-index table)
    (case way
      ((west) (swap-cell! table x y (- x 1) y))
      ((north) (swap-cell! table x y x (- y 1)))
      ((east) (swap-cell! table x y (+ x 1) y))
      ((south) (swap-cell! table x y x (+ y 1)))
      (else (error "bad way" way)))
    table))

(define (can-move? table way)
  (receive (west-ok? north-ok? east-ok? south-ok?)
      (available-ways table)
    (case way
      ((west) west-ok?)
      ((north) north-ok?)
      ((east) east-ok?)
      ((south) south-ok?)
      (else #f))))

(define (swap-cell! table x1 y1 x2 y2)
  (let* ((value1 (get-cell table x1 y1))
         (value2 (get-cell table x2 y2)))
    (set-cell! table x1 y1 value2)
    (set-cell! table x2 y2 value1)
    table))

(define (get-cell table x y)
  (ref (ref table y) x))

(define (set-cell! table x y value)
  (set! (ref (ref table y) x) value)
  table)

(define (clear? table)
  (equal? (sort-numbers (flatten (make-number-table (row-number-of table))))
          (flatten table)))

(define (shuffle-table! table)
  (let* ((max-num (row-number-of table))
         (shuffle-time (* max-num max-num max-num
                          (+ 1 (random-integer max-num))))
         (way-table (hash-table 'eqv?
                                '(0 . west)
                                '(1 . north)
                                '(2 . east)
                                '(3 . south))))
    (dotimes (i shuffle-time)
      (call-with-values (lambda () (available-ways table))
        (lambda ways
          (let ((way (random-integer (length ways))))
            (if (list-ref ways way)
                (move! table
                       (hash-table-get way-table way))))))))
  table)

(provide "number/table")