(define-module magic.server
  (extend dsm.server magic.magic)
  (use msm.marshal)
  (use dsm.common)
  (use magic.servlet)
  (export make-magic-server add-mount-point! get-by-mount-point
          magic-server-start! magic-server-join! magic-server-stop!))
(select-module magic.server)

(define-class <magic-server> ()
  ((dsm-server :accessor dsm-server-of)))

(define-method initialize ((self <magic-server>) args)
  (next-method)
  (set! (dsm-server-of self) (apply make-dsm-server args)))

(define (make-magic-server uri . keywords)
  (apply make <magic-server> uri keywords))

(define-method add-mount-point! ((self <magic-server>) mount-point servlet)
  (add-mount-point! (dsm-server-of self) mount-point
                    (cut dispatch servlet <> <> <> <...>)))

(define-method get-by-mount-point ((self <magic-server>) mount-point)
  (get-by-mount-point (dsm-server-of self) mount-point))

(define (magic-server-start! server)
  (dsm-server-start! (dsm-server-of server)))

(define (magic-server-join! server)
  (dsm-server-join! (dsm-server-of server)))

(define (magic-server-stop! server)
  (dsm-server-stop! (dsm-server-of server)))
  
(provide "magic/server")
