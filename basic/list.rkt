#lang typed/racket
;;(require/typed test-engine/racket-tests)
(require typed/rackunit)

;; A. match_ends
;; Given a list of strings, return the count of the number of
;; strings where the string length is 2 or more and the first
;; and last chars of the string are the same.

(: match-ends (-> (Listof String) Integer))
(define (match-ends words)
  (: first-char (-> String Char))
  (define (first-char s)
    (string-ref s 0))

  (: last-char (-> String Char))
  (define (last-char s)
    (string-ref s (sub1 (string-length s))))

  (length (filter (λ: ([w : String]) (and (<= 2 (string-length w))
                              (char=? (first-char w)
                                      (last-char w))))
                  words)))

(check-equal? (match-ends '("aba" "xyz" "aa" "x" "bbb")) 3)
(check-equal? (match-ends '("" "x" "xy" "xyx" "xx"))     2)
(check-equal? (match-ends '("aaa" "be" "abc" "hello"))   1)

;; B. front_x
;; Given a list of strings, return a list with the strings
;; in sorted order, except group all the strings that begin with 'x' first.
;; e.g. ['mix', 'xyz', 'apple', 'xanadu', 'aardvark'] yields
;; ['xanadu', 'xyz', 'aardvark', 'apple', 'mix']
;; Hint: this can be done by making 2 lists and sorting each of them
;; before combining them.

(: front-x (-> (Listof String) (Listof String)))
(define (front-x lst)
  (let-values ([(xs as)
                (partition (λ: ([s : String]) (char=? (string-ref s 0) #\x))
                           lst)])
    (append (sort xs string<?) (sort as string<?))))

(check-equal? (front-x '("bbb" "ccc" "axx" "xzz" "xaa"))
              '("xaa" "xzz" "axx" "bbb" "ccc"))

(check-equal? (front-x '("ccc" "bbb" "aaa" "xcc" "xaa"))
              '("xaa" "xcc" "aaa" "bbb" "ccc"))

(check-equal? (front-x '("mix" "xyz" "apple" "xanadu" "aardvark"))
              '("xanadu" "xyz" "aardvark" "apple" "mix"))

;; C. sort_last
;; Given a list of non-empty tuples, return a list sorted in increasing
;; order by the last element in each tuple.
;; e.g. [(1, 7), (1, 3), (3, 4, 5), (2, 2)] yields
;; [(2, 2), (1, 3), (3, 4, 5), (1, 7)]
;; Hint: use a custom key= function to extract the last element form each tuple.

;; assume input is list of lists
(: sort-last (-> (Listof (Listof Number)) (Listof (Listof Number))))
(define (sort-last lst)
  ((inst sort (Listof Number) Number) lst < #:key last))

(check-equal? (sort-last '((1 3) (3 2) (2 1)))
              '((2 1) (3 2) (1 3)))
(check-equal? (sort-last '((2 3) (1 2) (3 1)))
              '((3 1) (1 2) (2 3)))
(check-equal? (sort-last '((1 7) (1 3) (3 4 5) (2 2)))
              '((2 2) (1 3) (3 4 5) (1 7)))


;; D. Given a list of numbers, return a list where
;; all adjacent == elements have been reduced to a single element,
;; so [1, 2, 2, 3] returns [1, 2, 3]. You may create a new list or
;; modify the passed in list.

(: remove-adjacent (-> (Listof Integer) (Listof Integer)))
(define (remove-adjacent lst)
  (match lst
    ['() lst]
    [`(,a) lst]
    [`(,a ,b ,c ...)
     (if (= a b)
         (remove-adjacent (cdr lst))
         (cons a (remove-adjacent (cdr lst))))]))


(check-equal? (remove-adjacent '(1 2 2 3)) '(1 2 3))
(check-equal? (remove-adjacent '(2 2 3 3 3)) '(2 3))
(check-equal? (remove-adjacent '()) '())

;; E. Given two lists sorted in increasing order, create and return a merged
;; list of all the elements in sorted order. You may modify the passed in lists.
;; Ideally, the solution should work in "linear" time, making a single
;; pass of both lists.

(: linear-merge (-> (Listof String) (Listof String) (Listof String)))
(define/match (linear-merge l1 l2)
  [(l1 '()) l1]
  [('() l2) l2]
  [(`(,a ,as ...) `(,b ,bs ...))
   (if (string<? a b)
       (cons a (linear-merge (cdr l1) l2)) ; can't infer as and bs is [String]...
       (cons b (linear-merge l1 (cdr l2))))])

(check-equal?  (linear-merge '("aa" "xx" "zz") '("bb" "cc"))
               '("aa" "bb" "cc" "xx" "zz"))
(check-equal?  (linear-merge '("aa" "xx") '("bb" "cc" "zz"))
               '("aa" "bb" "cc" "xx" "zz"))
(check-equal?  (linear-merge '("aa" "aa") '("aa" "bb" "bb"))
               '("aa" "aa" "aa" "bb" "bb"))
