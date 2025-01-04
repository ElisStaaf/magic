(define-module number-table-servlet.view.http
  (use text.html-lite)
  (use text.tree)
  (use gauche.parameter)
  (use srfi-1)
  (use esm.gauche)
  (use magic.view.http)
  (use number-table-servlet.clear-list)
  (export jump-to-main)
  )
(select-module number-table-servlet.view.http)

(define-macro (reload-files)
  '(load-esm-files "number-table-servlet/view/http/*.esm"))

(reload-files)

(define (default-view)
  (main))

(define (jump-to-main)
  (set-response-value!
    :location (href :action 'main
		    *magic-user-key* (get-param *magic-user-key*)
		    *magic-password-key* (get-param *magic-password-key*)))
  "")

(define (available-ways)
  (map (lambda (way available)
         (list way available))
       '("west" "north" "east" "south")
       (get-value 'available-ways)))

(define (move-navi)
  (navigator :params (available-ways)))

(define (navi-link way)
  (if (get-param way)
      (tree->string
       (html:a :href (href :action "move" 'way way)
               way))
      way))

(define (get-clear-list . do-sort)
  (let ((lst (get-servlet-value 'clear-list '())))
    (if (get-optional do-sort #t)
        (sort lst (lambda (x y)
                    (< (clear-list-score x)
                       (clear-list-score y))))
        lst)))

(provide "number-table-servlet/view/http")
