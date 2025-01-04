(define-module magic.user.view.http
  (extend magic.common)
  (use magic.view.http)
  (export login-form)
  )
(select-module magic.user.view.http)

(define (login-form . attrs)
  (let-values (((id action attrs) (apply generate-id&action attrs)))
    (string-join `(,(apply form attrs)
                   ,@(map (lambda (alist)
                            (string-append
                             "\t<input type=\"hidden\" "
                             (string-join (alist->attributes alist)
                                          " ")
                             " />"))
                          `(((name ,*magic-id-key*)
                             (value ,id))
                            ((name ,*magic-action-key*)
                             (value ,action)))
                          ))
                 "\n")))

(provide "magic/user/view/http")
