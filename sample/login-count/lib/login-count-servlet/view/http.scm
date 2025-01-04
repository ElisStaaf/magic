(define-module login-count-servlet.view.http
  (use gauche.parameter)
  (use esm.gauche)
  (use magic.view.http)
  (export jump-to-main))
(select-module login-count-servlet.view.http)

(load-esm-files "login-count-servlet/view/http/*.esm")

(define (default-view)
  (main))

(define (jump-to-main)
  (set-response-value!
    :location (href :action 'main
		    *magic-user-key* (get-param *magic-user-key*)
		    *magic-password-key* (get-param *magic-password-key*)))
  "")

(provide "login-count-servlet/view/http")
