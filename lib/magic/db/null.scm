(define-module magic.db.null
  (extend magic.db)
  (export <magic-db-null>))
(select-module magic.db.null)

(define-class <magic-db-null> ()
  ())

(define-method store ((self <magic-db-null>))
  #f)

(define-method restore ((self <magic-db-null>))
  #f)

(define-method get-value ((self <magic-db-null>) key . default)
  (get-optional default #f))
  
(define-method set-value! ((self <magic-db-null>) key value)
  #f)
  
(define-method remove-value! ((self <magic-db-null>) key)
  #f)

(define-method value-exists! ((self <magic-db-null>) key)
  #f)

(provide "magic/db/null")
