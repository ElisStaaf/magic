#!/usr/bin/env gosh

(use test.unit)

(define-module magic.client.cgi.test
  (use test.unit)
  (use gauche.parameter)
  (use www.cgi)
  (require "test/util")
  (extend magic.client.cgi))
(select-module magic.client.cgi.test)

(define-test-case "cgi client test"
  ("test magic-mobile-agent?"
   (assert-each (lambda (user-agent)
                  (parameterize ((cgi-metavariables
                                  `(("HTTP_USER_AGENT" ,user-agent))))
                    (assert-true (and (magic-mobile-agent?) #t))))
                `("DoCoMo"
                  "J-PHONE"
                  "UP.Browser"
                  "DDIPOCKET"
                  "ASTEL"
                  "PDXGW"
                  "Palmscape"
                  "Xiino"
                  "sharp pda browser"
                  "Windows CE"
                  "L-mode"))
   (assert-each (lambda (user-agent)
                  (parameterize ((cgi-metavariables
                                  `(("HTTP_USER_AGENT" ,user-agent))))
                    (assert-false (magic-mobile-agent?))))
                `("Mozilla/5.0 (X11; U; Linux i686; ja-JP; rv:1.7.3) Gecko/20041007 Debian/1.7.3-5"
                  "w3m/0.5.1"
                  "Mozilla/5.0 (compatible; Konqueror/3.3; Linux) (KHTML, like Gecko)"))))
