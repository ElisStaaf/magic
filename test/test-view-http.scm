#!/usr/bin/env gosh

(use test.unit)
(use magic.view.http)

(require "test/util")

(define-test-case "magic http view test"
  ("h/hd test"
   (assert-each (lambda (expected str)
                  (assert-equal expected (hd str))
                  (assert-equal expected (hd (h expected)))
                  (assert-equal str (hd (h str))))
                '(("&" "&amp;")
                  ("\"" "&quot;")
                  (">" "&gt;")
                  ("<" "&lt;")
                  ("AAA<>BBB" "AAA&Lt;&gT;BBB")
                  ("a" "&#97;")
                  ("a" "&#097;")
                  ("a" "&#x61;")))))
