(define-module magic.servlet
  (extend magic.common)
  (use srfi-2)
  (use gauche.parameter)
  (use gauche.collection)
  (use gauche.threads)
  (use file.util)
  (use text.gettext)
  (use msm.marshal)
  (use dsm.server)
  (use magic.session)
  (use magic.user.manager)
  (use magic.db)
  (require "magic/view/http") ;; for thread, parameter and load/require
  (export <magic-servlet> dispatch db-of))
(select-module magic.servlet)

(autoload magic.user.manager.null <user-manager-null>)
(autoload magic.db.null <magic-db-null>)

(define-class <magic-servlet> ()
  ((session-constructor :accessor session-constructor-of
                        :init-keyword :session-constructor
                        :init-form make-magic-session)
   (servlet-module-name :accessor servlet-module-name-of
                        :init-keyword :servlet-module-name)
   (session-table :accessor session-table-of
                  :init-form (make-marshal-table))
   (session-store-filename :accessor session-store-filename-of
                           :init-form
                           (build-path *magic-default-working-directory*
                                       "session-db.scm"))
   (user-manager :accessor user-manager-of
                 :init-keyword :user-manager
                 :init-form (make <user-manager-null>))
   (db :accessor db-of :init-keyword :db
       :init-form (make <magic-db-null>))
   (domain :accessor domain-of :init-keyword :domain
           :init-value '("magic"))
   (gettext-dirs :accessor gettext-dirs-of :init-keyword :gettext-dirs
                 :init-value #f)
   (default-languages :accessor default-languages-of
                      :init-keyword :default-languages :init-value '())
   ))

(define-method initialize ((self <magic-servlet>) args)
  (next-method)
  (make-directory* (sys-dirname (session-store-filename-of self)))
  (restore self))

(define (get-module module-name)
  (or (find-module module-name)
      (begin
        ;; (p module-name)
        (require-in-root-thread (module-name->path module-name)
                                (current-module))
        (do () ((required-in-root-thread?))
          (thread-sleep! 0.5))
        (find-module module-name))))

(define-method dispatch ((self <magic-servlet>) id action type . args)
  (let ((sess (get-session self id)))
    (register-session! self sess)
    (parameterize ((session sess)
                   (parameters args)
                   (user-manager (user-manager-of self))
                   (servlet-db (db-of self)))
      (let ((langs (get-param *magic-language-key*
                              (default-languages-of self)
                              :list #t)))
        (parameterize ((servlet self)
                       (languages langs)
                       (app-gettext (make-gettext (domain-of self)
                                                  ;; I don't know why
                                                  ;; I need reverse!!!
                                                  (reverse langs)
                                                  (gettext-dirs-of self))))
          (clear! (session))
          (begin0
              (make-response self
                             (check-login-do (user-manager)
                                (or (get-param *magic-user-key* #f)
                                    (get-user))
                                (get-param *magic-password-key* #f)
                                action
                                (cut do-action self type <>)))
            (store self)))))))

(define (get-valid-session table id)
  (and (id-exists? table id)
       (let ((session (id-ref table id)))
         (if (valid-session? session)
           session
           (begin
             (id-delete! table id)
             #f)))))

(define (valid-session? session)
  (and session
       (magic-session? session)
       (valid? session)))

(define (get-session servlet id)
  (or (and-let* ((id)
                 (session (get-valid-session (session-table-of servlet) id)))
                session)
      ((session-constructor-of servlet))))

(define (register-session! servlet session)
  (let ((id (id-get (session-table-of servlet) session)))
    (set-id! session id)))

(define-method store ((self <magic-servlet>))
  (store (db-of self))
  (call-with-output-file (session-store-filename-of self)
    (cut store-session self <>)))

(define-method restore ((self <magic-servlet>))
  (restore (db-of self))
  (if (file-exists? (session-store-filename-of self))
      (call-with-input-file (session-store-filename-of self)
        (cut restore-session self <>))))

(define (store-session servlet out)
  (let ((table (session-table-of servlet)))
    (write (filter (lambda (elem)
                     (and (valid-session? (cdr elem))
                          (marshallable? (car elem))
                          (marshallable? (cdr elem))))
                   (marshal-table->alist table))
           out)))

(define (restore-session servlet in)
  (let ((data (read in)))
    (set! (session-table-of servlet)
          (alist->marshal-table (if (eof-object? data)
                                  '()
                                  data)))))

(define (eval-exported-proc module key default)
  (let ((table (module-table module)))
    (eval `(,(if (hash-table-exists? table key)
               key
               default))
          module)))

(define (do-action servlet type action)
  (make-result servlet
               type
               (eval-exported-proc
                (get-action-module servlet)
                (string->symbol #`"do-,|action|")
                (string->symbol #`"do-,|*magic-default-action-name*|"))))

(define (make-result servlet type view-name)
  (eval-exported-proc (get-response-module servlet type)
                      view-name
                      *magic-default-view-name*))

(define (get-action-module servlet)
  (get-module
   (string->symbol
    #`",(servlet-module-name-of servlet).action")))

(define (get-response-module servlet type)
  (get-module
   (string->symbol
    #`",(servlet-module-name-of servlet).view.,|type|")))

(define (make-response servlet result)
  (list (response-info-list (session))
        result))

(provide "magic/servlet")
