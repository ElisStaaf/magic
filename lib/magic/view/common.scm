(define-module magic.view.common
  (extend magic.common)
  (use file.util)
  (use esm.gauche)
  (export load-esm-files define-magic-esm
          define-magic-esm-from-string
          default-view)
  )
(select-module magic.view.common)

(define (default-view)
  "DEFAULT VIEW")

(define-macro (define-magic-esm name filename)
  `(define-magic-esm-from-string
     ,name
     (esm-result* ,(call-with-input-file filename port->string))))

(define-macro (define-magic-esm-from-string name src)
  (let ((args (gensym)))
    `(define (,name . ,args)
       (let-keywords* ,args ((params '()))
         (parameterize ((parameters `(,@params ,@(parameters))))
           ,src)))))

(define-macro (load-esm-files pattern)
  `(begin
     ,@(map (lambda (esm-filename)
              (rxmatch-let (rxmatch #/([^\/]+)\.[^.]+$/ esm-filename)
                  (orig name)
                (let ((proc-name (string->symbol name)))
                  `(begin
                     (export ,proc-name)
                     (define-magic-esm ,proc-name ,esm-filename)))))
            (call/cc
             (lambda (break)
               (for-each (lambda (path)
                           (let ((files (sys-glob (build-path path pattern))))
                             (if (not (null? files))
                                 (break files))))
                         *load-path*)
               '())))))

(provide "magic/view/common")
