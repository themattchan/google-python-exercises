#lang racket

(require (prefix-in h: html)
         (prefix-in x: xml)
         (prefix-in x: xml/path))

;; parsing xml with regex? are you nuts?
;; https://stackoverflow.com/questions/1732348/regex-match-open-tags-except-xhtml-self-contained-tags

(define read-html-as-xexpr
  (compose (curry map x:xml->xexpr)
           h:read-html-as-xml
           open-input-file))

(define (chunks-of n xs)
  (if (< (length xs) n)
      '()
      (let-values ([(p s) (split-at xs n)])
        (cons p (chunks-of n s)))))

; output: ['2006', 'Aaliyah 91', Aaron 57', 'Abagail 895', ' ...]
(define (count-names xs)  
  (let* ((cleaned
          (cons 'top
                (x:se-path*/list '(html body table tbody tr td) (cons 'html xs))))
         (date (string->number (last (string-split(x:se-path* '(top h3) cleaned)))))
         (sort1 (λ (xs) (sort xs string<? #:key car)))
         (names
          (sort1
           (apply append
             (map (match-lambda [`(,n ,male, female)
                                 (let ((n1 (string->number n)))
                                   `((,male . ,n1) (,female . ,n1)))])
               (chunks-of 3
                 (filter string?
                   (x:se-path*/list '(top table tbody tr td)
                                    cleaned))))))))
    (list date names)))

(define (t) (count-names (read-html-as-xexpr "baby2006.html")))
