(define-module magic.client.cgi
  (extend magic.client)
  (use www.cgi)
  (use gauche.charconv)
  (use gauche.regexp)
  (use gauche.parameter)
  (use srfi-1)
  (use srfi-13)
  (use rfc.822)
  (use rfc.cookie)
  (use util.list)
  (use text.tree)
  (use text.html-lite)
  (use dsm.client)
  (export magic-cgi-main magic-mobile-agent?))
(select-module magic.client.cgi)

(define (listify arg)
  (if (or (pair? arg) (null? arg)) arg (list arg)))
  
(define (add-meta-info params default-langs)
  (fold (lambda (key-info prev)
          (let* ((search-key (car key-info))
                 (regist-key (cadr key-info))
                 (value (with-module www.cgi
                          (if (pair? search-key)
                            (do ((keys search-key (cdr keys))
                                 (result #f
                                         (if (pair? (car keys))
                                           ((cadar keys) (get-meta (caar keys)))
                                           (get-meta (car keys)))))
                                ((or (null? keys)
                                     result)
                                 result)
                              ;; do nothing
                              )
                            (get-meta search-key)))))
            (if value
              (cons (cons regist-key (listify value))
                    prev)
              prev)))
        params
        `(((("REQUEST_URI" ,(lambda (value)
                              (and value
                                   (if (#/\?/ value)
                                     ((#/\?/ value) 'before)
                                     value))))
            "SCRIPT_NAME") "script-name")
          ("HTTP_HOST" "host-name")
          ((("HTTP_ACCEPT_LANGUAGE"
             ,(lambda (value)
                (append (cgi-get-parameter (x->string *magic-language-key*)
                                           params
                                           :default '() :list #t)
                        (parse-accept-language value default-langs)))))
           ,(x->string *magic-language-key*))
          ((("HTTP_IF_MODIFIED_SINCE"
             ,(lambda (value)
                (and value
                     (rfc822-date->date value)))))
           "if-modified-since"))))

(define (parse-accept-language value . default)
  (let ((langs (if (string? value)
                 (map (lambda (lang)
                        (string-trim-both
                         (car (string-split lang #\;))))
                      (string-split value #\,))
                 '())))
    (if (null? langs)
      (get-optional default #f)
      langs)))

(define (magic-cgi-main uri mount-point . args)
  (let-keywords* args ((error-proc (cut magic-error-proc <>
                                        (get-keyword :debug args #f)))
                       (default-langs '())
                       (output-encoding (cgi-output-character-encoding))
                       (default-parameters '())
                       (priority-parameters '())
                       (part-handlers '()))
    (cgi-main
     (lambda (params)
       (let* ((server (dsm-connect-server uri))
              (params (map (lambda (elem)
                             (map (lambda (x)
                                    (if (pair? x)
                                      (map (lambda (y)
                                             (ces-convert (x->string y) "*JP"))
                                           x)
                                      (ces-convert (x->string x) "*JP")))
                                  elem))
                           `(,@priority-parameters
                             ,@params
                             ,@default-parameters)))
              (dispatch (server mount-point))
              (id (cgi-get-parameter (x->string *magic-id-key*)
                                     params
                                     :convert string->number))
              (action (cgi-get-parameter (x->string *magic-action-key*)
                                         params
                                         :convert string->symbol
                                         :default ""))
              (type 'http)
              (result (apply dispatch id action type
                             (add-meta-info params default-langs)))
              (header-info (car result))
              (body (tree->string (cadr result)))
              (cookies (construct-cookie-string
                        `((,(x->string *magic-id-key*)
                           ,(x->string
                             (get-keyword *magic-id-key* header-info id))))))
              (status (get-keyword :status header-info "OK"))
              (content-type
               (get-keyword :content-type header-info
                            (format "text/html; charset=~a"
                                    (normalize-charset output-encoding))))
              (content-encoding
               (get-keyword :content-encoding header-info #f))
              (content-length
               (get-keyword :content-length header-info #f))
              (content-disposition
               (get-keyword :content-disposition header-info #f))
              (output-encoding
               (rxmatch-if (rxmatch #/\S+\;\s*charset=(\S+)/i content-type)
                   (_ charset)
                 charset
                 output-encoding)))
         (unless content-encoding
           (cgi-output-character-encoding output-encoding))
         `(,(cond ((get-keyword :location header-info #f)
                   => (cut make-cgi-header
                           :cookies cookies
                           :status "MOVED"
                           :Location <>))
                  (else
                   (list
                    (apply make-cgi-header
                           :cookies cookies
                           :status status
                           :content-type content-type
                           ;; :Content-Length (string-size body)
                           `(,@(if content-disposition
                                 `(:Content-Disposition ,content-disposition)
                                 '())
                             ,@(if content-length
                                 `(:Content-Length ,content-length)
                                 '())
                             ,@(if content-encoding
                                 `(:Content-Encoding ,content-encoding)
                                 '())))
                    body))))))
     :merge-cookies #t
     :on-error error-proc
     :part-handlers part-handlers)))

(define http-status-alist
  '(("OK" "200 OK")
    ("PARTIAL_CONTENT" "206 Partial Content")
    ("MULTIPLE_CHOICES" "300 Multiple Choices")
    ("MOVED" "301 Moved Permanently")
    ("REDIRECT" "302 Found")
    ("NOT_MODIFIED" "304 Not Modified")
    ("BAD_REQUEST" "400 Bad Request")
    ("AUTH_REQUIRED" "401 Authorization Required")
    ("FORBIDDEN" "403 Forbidden")
    ("NOT_FOUND" "404 Not Found")
    ("METHOD_NOT_ALLOWED" "405 Method Not Allowed")
    ("NOT_ACCEPTABLE" "406 Not Acceptable")
    ("LENGTH_REQUIRED" "411 Length Required")
    ("PRECONDITION_FAILED" "412 Precondition Failed")
    ("SERVER_ERROR" "500 Internal Server Error")
    ("NOT_IMPLEMENTED" "501 Method Not Implemented")
    ("BAD_GATEWAY" "502 Bad Gateway")
    ("VARIANT_ALSO_VARIES" "506 Variant Also Negotiates")))

(define (replace-keyword new-value keyword-list)
  (append-map (lambda (key&value)
                (let ((key (car key&value)))
                  (let ((new (assq key new-value)))
                    (if new
                      (cdr new)
                      key&value))))
              (slices keyword-list 2)))

(define (make-cgi-header . args)
  (let-keywords* args ((status "OK"))
    (let ((status (cond ((assoc status http-status-alist)
                         => cadr)
                        (else
                         status))))
      (apply cgi-header
             (replace-keyword `((:status :Status ,status))
                              args)))))

(define (magic-error-proc e debug)
  `(,(cgi-header)
    ,(html-doctype)
    ,(html:html
      (html:head (html:title "Error"))
      (html:body (html:h1 "Error")
                 (html:p (html-escape-string (slot-ref e 'message)))
                 (if debug
                     (html:pre (html-escape-string
                                (call-with-output-string
                                 (cut with-error-to-port <>
                                      (cut report-error e)))))
                     '())
                 (html:hr)
                 (html:p (html-escape-string
                          (format "Using magic version is ~a"
                                  *magic-version*)))
                 ))))

(define (normalize-charset charset)
  (rxmatch-case charset
    (#/eucjp/i (orig) "EUC-JP")
    (else charset)))

(define (magic-mobile-agent?)
  (#/(DoCoMo|J-PHONE|UP\.Browser|DDIPOCKET|ASTEL|PDXGW|Palmscape|Xiino|sharp pda browser|Windows CE|L-mode)/i
     (or (cgi-get-metavariable "HTTP_USER_AGENT")
         "")))

(provide "magic/client/cgi")
