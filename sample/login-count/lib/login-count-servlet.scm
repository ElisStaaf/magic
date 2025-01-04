(define-module login-count-servlet
  (use magic.servlet)
  (use magic.session)
  (use magic.user.manager.file)
  (use magic.db.file)
  (export make-login-count-servlet))
(select-module login-count-servlet)

(define (make-login-count-servlet)
  (let ((db (make <magic-db-file>)))
    (set-value! db 'count 0)
    (make <magic-servlet>
      :servlet-module-name 'login-count-servlet
      :session-constructor (lambda () (make-magic-session :count 0))
      :db db
      :user-manager (make <user-manager-file>
                      :default-authority 'deny
                      :authority-map '((#t #t)
                                       (#f login))))))

(provide "login-count-servlet")
