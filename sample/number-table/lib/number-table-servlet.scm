(define-module number-table-servlet
  (use srfi-2)
  (use gauche.parameter)
  (use magic.servlet)
  (use magic.session)
  (use magic.user.manager.file)
  (use magic.db.file)
  (use number-table)
  (use number-table-servlet.action)
  (export make-number-table-servlet))
(select-module number-table-servlet)

(define (make-number-table-servlet)
  (make <magic-servlet>
    :servlet-module-name 'number-table-servlet
    :session-constructor make-session
    :db (make <magic-db-file>)
    :user-manager (make <user-manager-file>
                    :default-authority 'deny
                    :authority-map '((#t #t)
                                     (#f add-user)))))

(define (make-session)
  (let ((table (make-number-table 3)))
    (shuffle-table! table)
    (let ((sess (make-magic-session :table table
                                      :count 0)))
      (parameterize ((session sess))
        (update-value!))
      sess)))

(provide "number-table-servlet")
