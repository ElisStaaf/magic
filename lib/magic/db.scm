(define-module magic.db
  (extend magic.common)
  (export store restore
          get-value set-value! remove-value! value-exists?))
(select-module magic.db)

(define-class <magic-db> ()
  ())

(define-method store ((self <magic-db>))
  (error "not implemented"))

(define-method restore ((self <magic-db>))
  (error "not implemented"))

(define-method get-value ((self <magic-db>) key . default)
  (error "not implemented"))
  
(define-method set-value! ((self <magic-db>) key value)
  (error "not implemented"))
  
(define-method remove-value! ((self <magic-db>) key)
  (error "not implemented"))
  
(define-method value-exists? ((self <magic-db>) key)
  (error "not implemented"))
  
(provide "magic/db")
