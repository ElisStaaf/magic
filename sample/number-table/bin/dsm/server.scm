#!/usr/bin/env gosh

(use dsm.server)
(use number-table)

(define *port* 5976)

(define (main args)
  (let* ((server (make-dsm-server #`"dsmp://:,|*port*|"))
         (marshal-table (with-module dsm.server (marshal-table-of server)))
         (id-get (with-module msm.marshal id-get))
         (id-ref (with-module msm.marshal id-ref)))

    (define (start-up)
      (let ((table (make-number-table 3))
            (count 0))
        (define (dispatch command . args)
          (case (string->symbol (x->string command))
            ((show) table)
            ((move)
             (if (null? args)
               "usage: move way"
               (let ((way (string->symbol (x->string (car args)))))
                 (if (can-move? table way)
                   (begin
                     (move! table way)
                     (inc! count)
                     table)
                   #`"can't move to the ,|way|"))))
            ((clear?) (clear? table))
            ((available-ways) (call-with-values
                                  (lambda () (available-ways table))
                                list))
            ((count) count)
            ((session-info) (id-get marshal-table dispatch))
            ((restore) (id-ref marshal-table (car args)))
            (else "bad command")))

        (shuffle-table! table)
        dispatch))

    (add-mount-point! server "/start" start-up)
    (dsm-server-start! server)
    (dsm-server-join! server)))
