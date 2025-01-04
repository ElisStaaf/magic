(define-module magic.db.file
  (extend magic.db)
  (use msm.marshal)
  (use util.list)
  (use file.util)
  (use srfi-1)
  (use gauche.collection)
  (export <magic-db-file>))
(select-module magic.db.file)

(define-method make-db ()
  (make-hash-table 'equal?))

(define-method make-db ((null <null>))
  (make-db))

(define-method make-db ((alist <pair>))
  (alist->hash-table alist 'equal?))

(define-class <magic-db-file> (<magic-db>)
  ((filename :accessor filename-of
             :init-keyword :filename
             :init-form
             (build-path *magic-default-working-directory*
                         "servlet-db.scm"))
   (db :accessor db-of
       :init-thunk make-db)))

(define-method initialize ((self <magic-db-file>) args)
  (next-method)
  (make-directory* (sys-dirname (filename-of self))))

(define-method store ((self <magic-db-file>))
  (call-with-output-file (filename-of self)
    (lambda (out)
      (write (filter (lambda (elem)
                       (and (marshallable? (car elem))
                            (marshallable? (cdr elem))))
                     (hash-table->alist (db-of self)))
             out))))

(define-method restore ((self <magic-db-file>))
  (if (file-exists? (filename-of self))
      (call-with-input-file (filename-of self)
        (lambda (in)
          (let ((alist (read in)))
            (if (and (not (eof-object? alist))
                     (not (null? alist)))
                (set! (db-of self) (make-db alist))))))))

(define-method get-value ((self <magic-db-file>) key . default)
  (hash-table-get (db-of self) key (get-optional default #f)))
  
(define-method set-value! ((self <magic-db-file>) key value)
  (hash-table-put! (db-of self) key value))
  
(define-method remove-value! ((self <magic-db-file>) key)
  (hash-table-delete! (db-of self) key))

(define-method value-exists? ((self <magic-db-file>) key)
  (hash-table-exists? (db-of self) key))

(provide "magic/db/file")
