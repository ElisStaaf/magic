#!/usr/bin/env gosh

(use dsm.client)

(define *host* "localhost")
(define *port* 5976)

(define (display-result res)
  (if (pair? res)
      (for-each print res)
      (print res)))

(define (display-prompt count)
  (display #`",|count|> "))

(define (parse-input input)
  (let ((tokens (string-split input #/\s+/)))
    (values (car tokens)
            (cdr tokens))))

(define (main args)
  (set! (port-buffering (current-output-port)) :none)
  (let ((client (dsm-connect-server #`"dsmp://,|*host*|:,|*port*|")))
    (call/cc
     (lambda (return)
       (let ((intep ((client "/start")))
             (command #f)
             (args '()))
         (do ((res (intep "show") (apply intep command args)))
             ((intep "clear?") (print "CLEAR!!!!"))
           (display-result res)
           (display-prompt (intep "count"))
           (let ((input (read-line)))
             (when (eof-object? input)
               (newline)
               (print "Thank you.")
               (return 0))
             (set!-values (command args) (parse-input input)))
           (print command)))))))

