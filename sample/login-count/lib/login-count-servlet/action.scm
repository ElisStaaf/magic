(define-module login-count-servlet.action
  (use srfi-2)
  (use magic.db)
  (use magic.user.manager)
  (use magic.action)
  (export do-login do-main do-countup))
(select-module login-count-servlet.action)

(define (do-login)
  (or (and-let* ((user (get-param *magic-user-key*))
                 (user)
                 (password (get-param *magic-password-key*))
                 (password))
                (cond ((valid-user? user password) 'jump-to-main)
                      ((user-exists? user)
                       (set-cycle-value!
                        :message (if (user-exists? user)
                                     "password doesn't match"
                                     #`"user ,|user| doesn't exist"))
                       'login)
                      (else
                       (add-user! (user-manager) user password)
                       (set-cycle-value!
                        :message #`"user ,|user| added.")
                       'jump-to-main)))
      'login))

(define (do-main)
  (set-servlet-value! 'count (+ 1 (get-servlet-value 'count)))
  'main)

(define (do-countup)
  (set-value! 'count (+ 1 (get-value 'count)))
  'main)

(define (do-default)
  (do-main))

(define (do-deny)
  (do-login))

(provide "login-count-servlet.action")
