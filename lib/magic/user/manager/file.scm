(define-module magic.user.manager.file
  (extend magic.user.manager)
  (use file.util)
  (use magic.db)
  (use magic.db.file)
  (export <user-manager-file>))
(select-module magic.user.manager.file)

(define-class <user-manager-file> (<user-manager>)
  ((user-db :accessor user-db-of)))

(define-method initialize ((self <user-manager-file>) args)
  (let-keywords* args ((filename (build-path *magic-default-working-directory*
                                             "passwd.scm")))
    (set! (user-db-of self)
          (make <magic-db-file> :filename filename)))
  (next-method))

(define-method store ((self <user-manager-file>))
  (store (user-db-of self)))

(define-method restore ((self <user-manager-file>))
  (restore (user-db-of self)))

(define-method add-user! ((self <user-manager-file>) user password . keywords)
  (define (add)
    (let ((can-add? (and (string? user) (string? password))))
      (when can-add?
        (set-value! (user-db-of self)
                    user
                    (make-user-info 
                     (compute-pass-phrase (digest-type-of self)
                                          password)))
        (store self))
      can-add?))
  (when (not (string? user))
    (error "user must be string." user))
  (restore self)
  (let-keywords* keywords ((if-exists #f))
    (if (user-exists? self user)
        (case if-exists
          ((:update) (add))
          ((:error) (error "User already exists."))
          ((#f) #f))
        (add))))

(define-method remove-user! ((self <user-manager-file>) user . keywords)
  (define (remove)
    (remove-value! (user-db-of self) user)
    (store self)
    #t)
  (restore self)
  (let-keywords* keywords ((if-does-not-exist #f))
    (if (user-exists? self user)
        (remove)
        (case if-does-not-exist
          ((:error) (error "User does not exist."))
          ((#f) #f)))))

(define-method user-exists? ((self <user-manager-file>) user)
  (and (string? user)
       (value-exists? (user-db-of self) user)))

(define-method pass-phrase ((self <user-manager-file>) user)
  (if (user-exists? self user)
      (user-info-pass-phrase (get-value (user-db-of self) user))
      #f))

(define *user-info-version* 1)
(define (make-user-info pass-phrase)
  (list *user-info-version* pass-phrase))
(define (user-info-version user-info)
  (car user-info))
(define (user-info-pass-phrase user-info)
  (if (and (number? (user-info-version user-info))
           (= (user-info-version user-info)
              *user-info-version*))
      (cadr user-info)
      #f))

(provide "magic/user/manager/file")
