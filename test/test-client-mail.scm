#!/usr/bin/env gosh

(use test.unit)

(define-module magic.client.mail.test
  (use test.unit)
  (use gauche.charconv)
  (require "test/util")
  (extend magic.client.mail))
(select-module magic.client.mail.test)

(define-test-case "mail client test"
  ("test content-type->encoding"
   (assert-each (lambda (expect content-type . args)
                  (assert-equal expect
                                (apply content-type->encoding content-type
                                       args)))
                `(("iso-2022-jp" "charset=iso-2022-jp")
                  ("iso-2022-jp" "charset=ISO-2022-JP")
                  ("iso-2022-jp" "charset=\"iso-2022-jp\"")
                  ("iso-2022-jp" "" "iso-2022-jp")
                  ("iso-2022-jp" "" "ISO-2022-JP")
                  ("ascii" "")
                  ("ascii" "BAD CONTENT-TYPE")
                  ("ascii" "Text/Plain;")
                  ("ascii" "Text/Plain")
                  ("iso-2022-jp" "Text/Plain;charset=iso-2022-jp")
                  ("iso-2022-jp" "Text/Plain;  charset=iso-2022-jp aaa"))))
  ("test encoding->language"
   (assert-each (lambda (expect encoding default)
                  (assert-equal expect
                                (encoding->language encoding default)))
                `(("ja" "iso-2022-jp" "en")
                  ("ja" "ISO-2022-JP" "en")
                  ("ja" "EUC-JP" "en")
                  ("ja" "euc-JP" "en")
                  ("ja" "eucJP" "en")
                  ("en" "ascii" "ja")
                  ("en" "ASCII" "ja")
                  ("en" "iso-8859-1" "ja")
                  ("ja" "???" "ja")
                  ("en" "???" "en"))))
  ("test parse-body"
   (let ((default-param-name "body"))
     (assert-each (lambda (expect body)
                    (assert-equal expect
                                  (parse-body (open-input-string body)
                                              '()
                                              default-param-name)))
                  `(((("a" "b")) "a:b")
                    ((("a" "b")) "a: b  ")
                    ((("a" "b")
                      ("c" "d e f")) "a: b  \nc:  d e f  ")
                    (((,default-param-name "abcde")) "abcde")
                    (((,default-param-name "abcde\nfghij")) "abcde\nfghij")
                    ((("a" "abcde\nfghij")) "a:\nabcde\nfghij")
                    ((("a" "abcde\nfghij")
                      ("b" "xyz")) "a:\nabcde\nfghij\nb: xyz")
                    ((("a" "abcde\nfghij")
                      ("b" "xyz")
                      (,default-param-name "foo"))
                     "a:\nabcde\nfghij\nb: xyz\nfoo")
                    ((("a" "abcde")
                      (,default-param-name "xyz\nabc")) "a:abcde\nxyz\nabc")
                    ((("a" "b" "c")) "a:b\na:c")
                    ((("a" "b")
                      (,default-param-name "bcd\nefg"))
                     "a:b\nbcd\nefg")
                    ((("a" "b")
                      (,default-param-name "bcd\nefg"))
                     ,#`"a:b\n,|default-param-name|:\nbcd\nefg"))))
   (assert-equal `(("a" "b")
                   ("body" "bcd\nefg")
                   ,@(make-id&action-params 1 "commit"))
                 (parse-body (open-input-string "a:b\nbody:\nbcd\nefg")
                             (make-id&action-params 1 "commit")
                             "default")))
  ("test add-param"
   (assert-each (lambda (expect name value params)
                  (assert-equal expect (add-param name value params)))
                '(((("a" "b")) "a" "b" ())
                  ((("a" "b" "c")) "a" "b" (("a" "c"))))))
  ("test port->header-list&body&language"
   (assert-each (lambda (expect-header expect-body expect-lang mail)
                  (receive (header body lang)
                      (port->header-list&body&language
                       (if (port? mail)
                         mail
                         (open-input-string mail)))
                    (assert-equal expect-header header)
                    (assert-equal expect-body body)
                    (assert-equal expect-lang lang)))
                `((() "" "en" "")
                  ((("subject" "abc")) "" "en" "Subject: abc")
                  ((("subject" "abc")) "" "en" "Subject: abc  ")
                  ((("subject" "abc")) "xyz" "en" "Subject: abc\n\nxyz")
                  ((("subject" "あabc")) "xyz" "en"
                   "Subject: =?iso-2022-jp?Q?=1B=24=42=24=22=1B=28=42?=abc\n\nxyz")
                  ((("subject" "abc")
                    ("content-type" "text/plain; charset=iso-2022-jp"))
                   "あいうabc"
                   "ja"
                   ,(ces-convert
                     (string-join
                      (list
                       "Subject: abc"
                       "Content-Type: text/plain; charset=iso-2022-jp"
                       ""
                       "あいうabc")
                      "\n")
                     "UTF-8" "iso-2022-jp")))))
  ("test id&action"
   (assert-each (lambda (expect-id expect-action str)
                  (receive (id action)
                      (id&action str)
                    (assert-equal expect-id id)
                    (assert-equal expect-action action)))
                '((0 "" "")
                  (3 "" "[3]")
                  (3 "abc" "[3]abc")
                  (0 "abc" "abc")
                  (0 "abc" "Re:   abc   ")
                  (33 "abc" "[ 33 ]  abc ")
                  (33 "abc" "  Re: [ 33 ] abc  ")))))

