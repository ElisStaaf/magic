(define-module magic.user.manager
  (extend magic.common)
  (use util.digest)
  (use file.util)
  (use magic.db)
  (export check-login-do
          store restore
          add-user! remove-user! user-exists?
          valid-user?))
(select-module magic.user.manager)

(autoload rfc.md5 <md5>)
(autoload rfc.sha1 <sha1>)

(define-class <user-manager> ()
  ((digest-type :accessor digest-type-of
                :init-keyword :digest-type
                :init-form <md5>)
   (default-authority :accessor default-authority-of
                      :init-keyword :default-authority
                      :init-value 'allow)
   (authority-map :accessor authority-map-of
                  :init-keyword :authority-map
                  :init-value '())))

(define-method initialize ((self <user-manager>) args)
  (next-method)
  (restore self))

(define-method check-login-do ((self <user-manager>)
                               user password request-action action-handler)
  (if (valid-user? self user password)
      (login! user))
  (action-handler (if (allow? self (get-user) request-action)
                      request-action
                      *magic-deny-action-name*)))

(define (allow? manager user action)
  (let ((allow (eq? 'allow (default-authority-of manager))))
    (if (need-invert? action (get-auth-info user (authority-map-of manager)))
        (not allow)
        allow)))

(define (get-auth-info user authority-map)
  (cond ((null? authority-map) '())
        ((eq? #f user)
         (if (eq? #f (caar authority-map))
             (cdar authority-map)
             (get-auth-info user (cdr authority-map))))
        ((memq (caar authority-map) (list #t user))
         (cdar authority-map))
        (else (get-auth-info user (cdr authority-map)))))

(define (need-invert? action auth-info)
  (cond ((null? auth-info) #f)
        ((memq (car auth-info) (list #t action)) #t)
        (else (need-invert? action (cdr auth-info)))))

(define-method store ((self <user-manager>))
  (error "not implemented"))

(define-method restore ((self <user-manager>))
  (error "not implemented"))

(define-method pass-phrase ((self <user-manager>) user)
  (error "not implemented"))

(define-method add-user! ((self <user-manager>) uesr password . keywrods)
  (error "not implemented"))

(define-method remove-user! ((self <user-manager>) uesr . keywords)
  (error "not implemented"))

(define-method user-exists? ((self <user-manager>) user)
  (error "not implemented"))

(define-method valid-user? ((self <user-manager>) user password)
  (and (string? user)
       (string? password)
       (valid-password? (digest-type-of self)
                        (pass-phrase self user)
                        password)))

(define (valid-password? digest-type required-pass-phrase password)
  (and (string? required-pass-phrase)
       (string? password)
       (string=? required-pass-phrase
                 (compute-pass-phrase digest-type password))))

(define (compute-pass-phrase digest-type password)
  (digest-hexify (digest-string digest-type password)))

(provide "magic/user/manager")
