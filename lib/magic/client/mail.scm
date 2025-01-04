(define-module magic.client.mail
  (extend magic.client)
  (use srfi-1)
  (use srfi-8)
  (use srfi-11)
  (use srfi-13)
  (use gauche.charconv)
  (use gauche.net)
  (use rfc.822)
  (use rfc.base64)
  (use rfc.quoted-printable)
  (use dsm.client)
  (export magic-mail-main))
(select-module magic.client.mail)

(define (port->header-list&body&language in)
  (let* ((headers (map (lambda (field)
                         (list (car field)
                               (string-trim-right
                                (decode-field (cadr field)
                                              (gauche-character-encoding)))))
                       (rfc822-header->list in)))
         (encoding (content-type->encoding
                    (rfc822-header-ref headers "content-type" ""))))
    (values headers
            (call-with-input-conversion in
              port->string
              :encoding encoding)
            (encoding->language encoding "en"))))

(define (content-type->encoding ctype . default)
  (let ((md (rxmatch #/([^\;]+\;)?\s*charset=(\S+)/ ctype)))
    (string-trim-both
     (string-downcase (if md
                        (md 2)
                        (get-optional default "ascii")))
     #\")))

(define (encoding->language encoding default)
  (cond ((#/jp|jis|cp932/i encoding) "ja")
        ((#/ascii|iso-8859-1/i encoding) "en")
        (else default)))

(define (id&action str . defaults)
  (let-keywords* defaults ((id 0)
                           (action ""))
    (let ((md (rxmatch #/^\s*(?i:Re:)?\s*(?:\[\s*(\d+)?\s*\])?\s*(\S+)?\s*$/
                       str)))
      (values (if (and md (md 1))
                  (x->number (md 1))
                  id)
              (or (and md (md 2))
                  action)))))

(define (magic-mail-main uri mount-point mail . args)
  (let-keywords* args ((default-param-name "body")
                       (http-base #f))
    (let ((in (cond ((input-port? mail) mail)
                    ((string? mail) (open-input-string mail))
                    (else (error #`",mail must be input-port or string"))))
          (type 'smtp))
      (let*-values (((headers mail-body language)
                     (port->header-list&body&language in))
                    ((id action)
                     (id&action (rfc822-header-ref headers "subject" ""))))
        (let* ((server (dsm-connect-server uri))
               (dispatch (server mount-point))
               (result (apply dispatch id action type
                              (append-params
                               (parse-body
                                (open-input-string mail-body)
                                `((,(x->string *magic-language-key*)
                                   ,language)
                                  ("http-base" ,http-base)
                                  ,@(make-id&action-params id action))
                                default-param-name)
                               headers)))
               (header-info (car result))
               (body (open-input-string (cadr result))))
          (send-mail "localhost" 25 body
                     (get-keyword :from header-info
                                  (rfc822-header-ref headers "to"))
                     (get-keyword :to header-info
                                  (rfc822-header-ref headers "from")))
          0)))))

(define (add-param name value params)
  (let ((param (assoc name params)))
    (if param
        (begin
          (set! (cdr param) (cons value (cdr param)))
          params)
        (cons (list name value) params))))

(define (parse-body body params default-param-name)
  (define (add-line line in)
    (string-join (cons line (port->string-list in))
                 "\n"))

  (let ((line (read-line body)))
    (cond ((eof-object? line) params)
          ((rxmatch #/^(?:>\s*)?(\S+):\s*$/ line)
           => (lambda (md)
                (do ((next (read-line body) (read-line body))
                     (acc '() (cons next acc)))
                    ((or (eof-object? next)
                         (rxmatch #/^(?:>\s*)?(\S+):/ next))
                     (let ((name (md 1))
                           (value (string-join (reverse! acc) "\n")))
                       (if (eof-object? next)
                         `((,name ,value) ,@params)
                         (add-param name value
                                    (parse-body
                                     (open-input-string (add-line next body))
                                     params
                                     default-param-name))))))))
          ((rxmatch #/^(?:>\s*)?(\S+):\s*(.+)$/ line)
           => (lambda (md)
                (add-param (md 1) (string-trim-right (md 2))
                           (parse-body body params default-param-name))))
          (else (add-param default-param-name (add-line line body)
                           params)))))

(define (make-id&action-params id action)
  `((,(x->string *magic-id-key*) ,id)
    (,(x->string *magic-action-key*) ,action)))

(define (append-params p1 p2)
  (fold-right (lambda (param prev)
                (let ((name (car param)))
                  (fold-right (lambda (value params)
                                (add-param name value params))
                              prev
                              (cdr param))))
              p1
              p2))

;; from scmail
(define (send-mail host port iport mail-from recipients)
  (with-error-handler
   (lambda (e) (errorf "send-mail failed: ~a" (slot-ref e 'message)))
   (lambda ()
     (call-with-client-socket
      (make-client-socket 'inet host port)
      (lambda (in out)
	(let ((send-command 
	       (lambda (command code)
		 (when command (format out "~a\r\n" command))
		 (let* ((line (read-line in))
			(return-code (string->number (substring line 0 3))))
		   (if (eq? return-code code)
		       line
		       (errorf "smtp-error: ~a => ~a" command line))))))
	  (send-command #f 220)
	  (send-command (format "HELO ~a" (sys-gethostname)) 250)
	  (send-command (format "MAIL FROM: <~a>" mail-from) 250)
	  (for-each (lambda (rcpt)
		      (send-command (format "RCPT TO: <~a>" rcpt) 250))
		    (if (string? recipients) (list recipients) recipients))
	  (send-command "DATA" 354)
	  (port-for-each (lambda (line)
			   (format out "~a\r\n"
				   (regexp-replace #/^\./ line "..")))
			 (lambda () (read-line iport)))
	  (send-command "." 250)
	  (send-command "QUIT" 221)))))))

;; from scmail
(define (decode-field str to-code)
  (with-error-handler 
   (lambda (e) str)
   (lambda ()
     (regexp-replace-all #/=\?([^?]+)\?([BQ])\?([^?]+)\?=\s*/ 
			 str
			 (lambda (m)
			   (let* ((charcode (rxmatch-substring m 1))
				  (encoding (rxmatch-substring m 2))
				  (message  (rxmatch-substring m 3))
				  (decode (if (equal? encoding "B")
					      base64-decode-string
					      quoted-printable-decode-string)))
			     (ces-convert (decode message)
					  charcode
					  to-code)))))))

(provide "magic/client/mail")
