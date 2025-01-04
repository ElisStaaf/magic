(define-module where.servlet.view.http
  (use gauche.parameter)
  (use sxml.tools)
  (use text.tree)
  (use esm.gauche)
  (use magic.view.http))
(select-module where.servlet.view.http)

(use-cookie-only #f)

(load-esm-files "where/servlet/view/http/*.esm")

(define (move-to where)
  `(a (@ (href ,(href :action where)))
      ,where))

(define (move-links)
  (tree->string
   (sxml:sxml->html
    (map (lambda (where)
           (list "[" (move-to where) "]"))
         '("left" "center" "right")))))

(define (default-view)
  (show))

(provide "where/servlet/view/http")
