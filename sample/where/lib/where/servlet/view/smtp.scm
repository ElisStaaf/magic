(define-module where.servlet.view.smtp
  (use gauche.parameter)
  (use sxml.tools)
  (use text.tree)
  (use esm.gauche)
  (use magic.view.smtp))
(select-module where.servlet.view.smtp)

(load-esm-files "where/servlet/view/smtp/*.esm")

(define (move-links)
  (tree->string
   (map (lambda (where)
          (list "[" where "]"))
        '("left" "center" "right"))))

(define (default-view)
  (show))

(provide "where/servlet/view/smtp")
