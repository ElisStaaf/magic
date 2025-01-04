#!/usr/local/bin/gosh

(use text.tree)
(use text.html-lite)
(use www.cgi)
(use dsm.client)

(define *number-table-server* "localhost")
(define *number-table-port* 5976)

(define _html-escape-string html-escape-string)
(define (html-escape-string obj)
  (_html-escape-string (x->string obj)))

(define (number-table->html:table table)
  (html:table
   :border 1
   (map (lambda (row)
          (html:tr
           (map (lambda (cell)
                  (html:td (html-escape-string cell)))
                row)))
        table)))

(define (navigator id west north east south)
  (let ((params `((id . ,id))))
    (html:table
     :border 1
     (html:tr (html:td "")
              (html:td (if north
                           (navi-link "north" (cons '(way . north) params))
                           "north"))
              (html:td ""))
     (html:tr (html:td (if west
                           (navi-link "west" (cons '(way . west) params))
                           "west"))
              (html:td "")
              (html:td (if east
                           (navi-link "east" (cons '(way . east) params))
                           "east")))
     (html:tr (html:td "")
              (html:td (if south
                           (navi-link "south" (cons '(way . south) params))
                           "south"))
              (html:td "")))))

(define (navi-link title params)
  (html:a :href (tree->string `(,#`",|program-name|?"
                                ,(string-join
                                  (map (lambda (param)
                                         #`",(car param)=,(cdr param)")
                                       params)
                                  ";")))
          title))

(define program-name #f)

(define (main args)
  (cgi-main
   (lambda (params)
     (set! program-name (cgi-get-parameter "SCRIPT_NAME" params
                                           :default *program-name*))
     (let* ((server
             (dsm-connect-server
              #`"dsmp://,|*number-table-server*|:,|*number-table-port*|"))
            (intep ((server "/start")))
            (id (cgi-get-parameter "id" params :convert string->number))
            (way (cgi-get-parameter "way" params)))
       (if id
           (set! intep (intep "restore" id))
           (set! id (intep "session-info")))
       (if way (intep "move" way))
       `(,(cgi-header)
         ,(html-doctype)
         ,(html:html
           (html:head (html:title "Number Table Example"))
           (html:body
            (html:h1 "Number Table")
            (html:p (html:a :href program-name "NEW GAME")
                    "/"
                    (intep "count"))
            (if (intep "clear?") (html:h2 "CLEAR!!!") "")
            (number-table->html:table (intep "show"))
            (apply navigator id (intep "available-ways")))))))))
