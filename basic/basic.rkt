#lang racket
(require test-engine/racket-tests)

;;------------------------------------------------------------------------------
;; list1
;;------------------------------------------------------------------------------

;; A. match_ends
;; Given a list of strings, return the count of the number of
;; strings where the string length is 2 or more and the first
;; and last chars of the string are the same.
(define (match-ends words)
  (define (first-char s)
    (string-ref s 0))

  (define (last-char s)
    (string-ref s (sub1 (string-length s))))

  (length (filter (λ (w) (and (<= 2 (string-length w))
                              (char=? (first-char w)
                                      (last-char w))))
                  words)))

(check-expect (match-ends '("aba" "xyz" "aa" "x" "bbb")) 3)
(check-expect (match-ends '("" "x" "xy" "xyx" "xx"))     2)
(check-expect (match-ends '("aaa" "be" "abc" "hello"))   1)

;; B. front_x
;; Given a list of strings, return a list with the strings
;; in sorted order, except group all the strings that begin with 'x' first.
;; e.g. ['mix', 'xyz', 'apple', 'xanadu', 'aardvark'] yields
;; ['xanadu', 'xyz', 'aardvark', 'apple', 'mix']
;; Hint: this can be done by making 2 lists and sorting each of them
;; before combining them.

(define (front-x lst)
  (let-values ([(xs as) (partition (λ (s) (char=? (string-ref s 0) #\x)) lst)])
    (append (sort xs string<?) (sort as string<?))))

(check-expect (front-x '("bbb" "ccc" "axx" "xzz" "xaa"))
              '("xaa" "xzz" "axx" "bbb" "ccc"))

(check-expect (front-x '("ccc" "bbb" "aaa" "xcc" "xaa"))
              '("xaa" "xcc" "aaa" "bbb" "ccc"))

(check-expect (front-x '("mix" "xyz" "apple" "xanadu" "aardvark"))
              '("xanadu" "xyz" "aardvark" "apple" "mix"))

;; C. sort_last
;; Given a list of non-empty tuples, return a list sorted in increasing
;; order by the last element in each tuple.
;; e.g. [(1, 7), (1, 3), (3, 4, 5), (2, 2)] yields
;; [(2, 2), (1, 3), (3, 4, 5), (1, 7)]
;; Hint: use a custom key= function to extract the last element form each tuple.

;; assume input is list of lists
(define (sort-last lst)
  (sort lst < #:key last))

(check-expect (sort-last '((1 3) (3 2) (2 1)))
              '((2 1) (3 2) (1 3)))
(check-expect (sort-last '((2 3) (1 2) (3 1)))
              '((3 1) (1 2) (2 3)))
(check-expect (sort-last '((1 7) (1 3) (3 4 5) (2 2)))
              '((2 2) (1 3) (3 4 5) (1 7)))

(test)
