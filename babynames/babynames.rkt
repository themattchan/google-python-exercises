#lang racket

 (require (prefix-in h: html)
           (prefix-in x: xml))

;; parsing xml with regex? are you nuts?
;; https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags
(define read-html-as-xexpr
  (compose (curry map x:xml->xexpr)
           h:read-html-as-xml
           open-input-file))

(display(caddr (read-html-as-xexpr "baby1990.html")))



;(define (extract-names file))
