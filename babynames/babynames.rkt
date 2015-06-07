#lang racket

 (require (prefix-in h: html)
          (prefix-in x: xml))

;; parsing xml with regex? are you nuts?
;; https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags

(define read-html-as-xexpr
  (compose (curry map x:xml->xexpr)
           h:read-html-as-xml
           open-input-file))

;; gross
(define (get-info lst)
  (drop (car (drop
              (car (drop
                    (car (drop
                          (car (drop
                                (car (drop lst 2))
                                5))
                          2))
                    2))
              3))
        3))

(define (bfs pat tree )
  (match tree
    [`(,pat ...) (car tree)]
    ['()         (map (curry bfs pat) tree)]
    [_           (bfs (cdr tree) pat)]))

(define (dfs tree pat) ...)


;(define (extract-names file))
(bfs `(h3 ...) (get-info (read-html-as-xexpr "baby1990.html"))
