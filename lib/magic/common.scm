(define-module magic.common
  (extend magic.magic)
  (use www.cgi)
  (use srfi-1)
  (use srfi-11)
  (use srfi-29)
  (use text.gettext)
  (use gauche.parameter)
  (use gauche.sequence)
  (export *magic-id-key* *magic-user-key*
          *magic-password-key* *magic-action-key*
          *magic-action-not-specify* *magic-language-key*
          
          *magic-base-uri-key* *magic-base-key*
          
          session parameters user-manager servlet servlet-db
          app-gettext languages default-language
          
          get-param get-action get-user
          get-servlet-value set-servlet-value!
          delete-servlet-value! servlet-value-exists?
          get-value set-value! delete-value! value-exists?
          get-cycle-value set-cycle-value!
          delete-cycle-value! cycle-value-exists?
          get-response-value set-response-value!
          delete-response-value! response-value-exists?
          get-id set-id!

          generate-id&action&language

          get-base get-base-uri
          
          login! logout! login?
          valid-user? user-exists?
          
          _ n_ c_ nc_

          domain-of)
  )
(select-module magic.common)

(define *magic-id-key* :__magic_id__)
(define *magic-user-key* :__magic_user__)
(define *magic-password-key* :__magic_password__)
(define *magic-action-key* :__magic_action__)
(define *magic-language-key* :__magic_language__)

(define *magic-default-action-name* 'default)
(define *magic-default-view-name* 'default-view)
(define *magic-deny-action-name* 'deny)
(define *magic-new-session-id* 0)
(define *magic-action-not-specify* #f)

(define *magic-default-working-directory* ".state")

(define *magic-base-key* "base")
(define *magic-base-uri-key* "base-uri")

(define session (make-parameter #f))
(define parameters (make-parameter '()))
(define user-manager (make-parameter #f))
(define servlet (make-parameter #f))
(define servlet-db (make-parameter #f))
(define app-gettext (make-parameter #f))
(define languages (make-parameter #f))
(define default-language (make-parameter #f))

(define (get-param keyword . options)
  (apply cgi-get-parameter (x->string keyword) (parameters)
         `(,@(if (and (not (null? options))
                      (not (keyword? (car options))))
                 (cons :default options)
                 options)
           :list #f)))

(define-method get-servlet-value (key . fall-back)
  (apply get-value (servlet-db) key fall-back))
(define-method set-servlet-value! (key value)
  (set-value! (servlet-db) key value))
(define-method delete-servlet-value! (key)
  (delete-value! (servlet-db) key))
(define-method servlet-value-exists? (key)
  (value-exists? (servlet-db) key))

(define-method get-value (key . fall-back)
  (apply get-value (session) key fall-back))
(define-method set-value! (key value)
  (set-value! (session) key value))
(define-method delete-value! (key)
  (delete-value! (session) key))
(define-method value-exists? (key)
  (value-exists? (session) key))

(define-method get-cycle-value (key . fall-back)
  (apply get-cycle-value (session) key fall-back))
(define-method set-cycle-value! (key value)
  (set-cycle-value! (session) key value))
(define-method delete-cycle-value! (key)
  (delete-cycle-value! (session) key))
(define-method cycle-value-exists? (key)
  (cycle-value-exists? (session) key))

(define-method get-response-value (key . fall-back)
  (apply get-response-value (session) key fall-back))
(define-method set-response-value! (key value)
  (set-response-value! (session) key value))
(define-method delete-response-value! (key)
  (delete-response-value! (session) key))
(define-method response-value-exists? (key)
  (response-value-exists? (session) key))

(define-method get-id ()
  (get-id (session)))
(define-method set-id! (id)
  (set-id! (session) id))

(define (get-action)
  (get-param *magic-action-key*))

(define (get-user)
  (get-value *magic-user-key* #f))

(define (generate-id&action&language . args)
  (let ((default-id (get-id)))
    (let loop ((id default-id)
               (action (or (get-action)
                           *magic-default-action-name*))
               (language (if (null? (languages))
                           #f
                           (car (languages))))
               (args args))
      (cond ((null? args) (values id action language args))
            ((memq (car args) '(:new-session :action :language))
             => (lambda (keywords)
                  (case (car keywords)
                    ((:new-session) (loop (if (cadr args)
                                              *magic-new-session-id*
                                              default-id)
                                          action
                                          language
                                          (cddr args)))
                    ((:action) (loop id (cadr args) language (cddr args)))
                    ((:language) (loop id action (cadr args) (cddr args))))))
            (else (let ((ind (find-index (cut memq <>
                                              '(:new-session :action :language))
                                         args)))
                    (if ind
                        (loop id action language
                              (let-values (((before after) (split-at args ind)))
                                `(,@(take after 2)
                                  ,@before
                                  ,@(cddr after))))
                        (values id action language args))))))))

(define (get-base)
  (get-param *magic-base-key*))

(define (get-base-uri)
  (get-param *magic-base-uri-key*))


(define (login! user)
  (set-value! *magic-user-key* user))
(define (logout!)
  (set-value! *magic-user-key* #f))
(define (login? user)
  (let ((current-user (get-user)))
    (and (string? user)
         (string? current-user)
         (string=? user current-user))))

(define-method valid-user? (user password)
  (valid-user? (user-manager) user password))
(define-method user-exists? (user)
  (user-exists? (user-manager) user))

(define (_ msg-id)
  ((app-gettext) 'get msg-id))
(define (n_ msg-id . opt)
  (apply (app-gettext) 'nget msg-id opt))
(define (c_ msg-id locale)
  ((make-gettext (domain-of (servlet)) (list locale)) 'get msg-id))
(define (nc_ msg-id msg-id locale . opt)
  (apply (make-gettext (domain-of (servlet)) (list locale)) 'nget msg-id opt))

(define-method domain-of ()
  (error "dummy method"))

(provide "magic/common")
