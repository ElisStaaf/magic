(define-module magic.user.manager.null
  (extend magic.user.manager)
  (export <user-manager-null>))
(select-module magic.user.manager.null)

(define-class <user-manager-null> ()
  ())

(define-method check-login-do ((self <user-manager-null>)
                               user password request-action action-handler)
  (action-handler request-action))

(define-method store ((self <user-manager-null>))
  ;; do nothing
  #f)

(define-method restore ((self <user-manager-null>))
  ;; do nothing
  #f)

(define-method pass-phrase ((self <user-manager-null>) user)
  ;; do nothing
  #f)

(define-method add-user! ((self <user-manager-null>) uesr password . keywrods)
  ;; do nothing
  #f)

(define-method remove-user! ((self <user-manager-null>) uesr . keywords)
  ;; do nothing
  #f)

(define-method user-exists? ((self <user-manager-null>) user)
  ;; do nothing
  #f)

(provide "magic/user/manager/null")
