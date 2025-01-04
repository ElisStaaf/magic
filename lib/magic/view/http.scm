(define-module magic.view.http
  (extend magic.view.common)
  (use srfi-11)
  (use rfc.uri)
  (use gauche.regexp)
  (use gauche.parameter)
  (use file.util)
  (use util.list)
  (use text.tree)
  (use text.html-lite)
  (export use-cookie-only default-action
          form input
          h hd u ue
          href full-href alist->attributes
          user-name-input password-input
          language-select))
(select-module magic.view.http)

(define use-cookie-only (make-parameter #t))
(define default-action (make-parameter #f))

(define (default-view)
  (html:html
   (html:head
    (html:title "DEFAULT VIEW"))
   (html:body
    (html:h1 "DEFUALT VIEW")
    (html:p "This is default view. You must overwrite this."))))

(define (h str)
  (with-string-io (x->string str) html-escape))

(define (hd str)
  (regexp-replace-all #/&(.*?)\;/ str
    (lambda (md)
      (rxmatch-case (md 1)
        (#/^amp$/i (#f) "&")
        (#/^quot$/i (#f) "\"")
        (#/^gt$/i (#f) ">")
        (#/^lt$/i (#f) "<")
        (#/^#0*(\d+)$/i (#f int)
          (ucs->char (string->number int)))
        (#/^#x([0-9a-f]+)$/i (#f hex)
          (ucs->char (string->number hex 16)))
        (else #`"&,(md 1);")))))
  
(define u uri-encode-string)
(define ud uri-decode-string)

(define (alist->params alist)
  (map (lambda (elem)
         #`",(car elem)=,(h (cadr elem))")
       alist))

(define (params->string params)
  (string-join params ";"))

(define (alist->params-string alist)
  (params->string (alist->params alist)))

(define (alist->attributes alist)
  (map (lambda (elem)
         #`",(car elem)=\",(h (cadr elem))\"")
       alist))

(define (attributes->string attrs)
  (string-join attrs " "))

(define (alist->attributes-string alist)
  (attributes->string (alist->attributes alist)))

(define (href . params)
  (let ((new-session? (get-keyword :new-session params #f)))
    (let-values (((id action language params)
                  (apply generate-id&action&language params)))
      (h (string-append (or (get-base)
                            (get-param "script-name" ""))
                        "?"
                        (alist->params-string
                         `(,@(if (and (not new-session?)
                                      (use-cookie-only))
                               '()
                               (list (list *magic-id-key* id)))
                           ,@(if (or (equal? action
                                             *magic-default-action-name*)
                                     (equal? action
                                             (default-action)))
                               '()
                               (list (list *magic-action-key* action)))
                           ,@(if language
                               (list (list *magic-language-key* language))
                               '())
                           ,@(slices params 2))))))))

(define (full-href . params)
  (let ((base-uri (get-base-uri)))
    (if base-uri
      #`",|base-uri|,(apply href params)"
      (let ((host (get-param "host-name" "localhost")))
        #`"http://,|host|,(apply href params)"))))

(define (input . keywords)
  (tree->string `("<input "
                  ,(alist->attributes-string (slices keywords 2))
                  " />")))

(define (form . attrs)
  (let ((new-session? (get-keyword :new-session attrs #f)))
    (let-values (((id action language attrs)
                  (apply generate-id&action&language attrs)))
      (tree->string `("<form action=\""
                      ,(get-param "script-name" "")
                      "\" "
                      ,(alist->attributes-string (slices attrs 2))
                      ">\n"
                      ,@(if (and (not new-session?)
                                 (use-cookie-only))
                          '()
                          (list (input :type 'hidden
                                       :name *magic-id-key*
                                       :value id)))
                      ,@(if (or (equal? action *magic-default-action-name*)
                                (equal? action (default-action))
                                (equal? action *magic-action-not-specify*))
                          '()
                          (list (input :type 'hidden
                                       :name *magic-action-key*
                                       :value action)))
                      ,@(if language
                          (list (input :type 'hidden
                                       :name *magic-language-key*
                                       :value language))
                          '()))))))

(define (language-select current-lang langs . args)
  (let-keywords* args ((key *magic-language-key*)
                       (use-onchange-submit? #t))
    (tree->string `("<select name=\""
                    ,key
                  "\""
                  (if use-onchange-submit?
                    " onchange=\"this.form.submit();\""
                    "")
                  ">\n"
                  ,@(map (lambda (lang)
                           `("<option value=\"" ,(h lang) "\""
                             ,(if (equal? current-lang lang)
                                " selected=\"selected\""
                                "")
                             ">"
                             ,(h (_ lang))
                             "(" ,(h lang) ")"
                             "</option>\n"))
                         langs)
                  "</select>\n"))))

(define (user-name-input . attrs)
  (apply input
         :type 'text
         :name *magic-user-key*
         :value (or (get-param *magic-user-key* #f)
                    (get-user)
                    "")
         attrs))

(define (password-input . attrs)
  (apply input
         :type 'password
         :name *magic-password-key*
         attrs))

(provide "magic/view/http")
