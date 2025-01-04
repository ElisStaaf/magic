(define-module number-table-servlet.action
  (use srfi-2)
  (use magic.action)
  (use magic.user.manager)
  (use number-table)
  (use number-table-servlet.clear-list)
  (export update-value! do-move do-deny do-main))
(select-module number-table-servlet.action)

(define (get-available-ways table)
  (call-with-values
      (lambda () (available-ways table))
    list))

(define (table)
  (get-value 'table))

(define (count)
  (get-value 'count))

(define (game-clear?)
  (get-value 'clear?))

(define (update-value!)
  (set-value! 'clear? (clear? (table)))
  (set-value! 'available-ways (get-available-ways (table))))

(define (do-default)
  (do-main))

(define (do-main)
  (if (game-clear?)
      (begin
        (set-servlet-value! 'clear-list
                            (cons (make-clear-list (get-user) (count))
                                  (get-servlet-value 'clear-list '())))
        'clear-list)
      'main))

(define (do-move)
  (let* ((way (get-param "way" :convert string->symbol)))
    (set-value! 'count (+ 1 (count)))
    (if (can-move? (table) way)
      (move! (table) way)
      (set-cycle-value!
       :message #`"can't move to the ,|way|."))
    (update-value!)
    (do-main)))

(define (do-add-user)
  (or (and-let* ((user (get-param *magic-user-key*))
                 (user)
                 (password (get-param *magic-password-key*))
                 (password))
                (if (user-exists? (user-manager) user)
                    (begin
                      (set-cycle-value!
                       :message #`"user ,|user| already exists.")
                      'add-user)
                    (if (add-user! (user-manager) user password)
                        (begin
                          (set-cycle-value!
                           :message #`"user ,|user| added.")
                          'login)
                        (begin
                          (set-cycle-value!
                           :message #`"user ,|user| can't add.")
                          'add-user))))
      'add-user))

(define (do-deny)
  (do-login))

(define (do-login)
  (or (and-let* ((user (get-param *magic-user-key*))
                 (user)
                 (password (get-param *magic-password-key*))
                 (password))
                (if (valid-user? user password)
                    'jump-to-main
                    (begin
                      (set-cycle-value!
                       :message (if (user-exists? user)
                                    "password doesn't match"
                                    #`"user ,|user| doesn't exist"))
                      'login)))
      'login))

(provide "number-table-servlet/action")
