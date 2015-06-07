#lang racket
(require test-engine/racket-tests)

;; A. donuts
;; Given an int count of a number of donuts, return a string
;; of the form 'Number of donuts: <count>', where <count> is the number
;; passed in. However, if the count is 10 or more, then use the word 'many'
;; instead of the actual count.
;; So donuts(5) returns 'Number of donuts: 5'
;; and donuts(23) returns 'Number of donuts: many'

(define (donuts i)
  (~a "Number of donuts: " (if (< i 10) i "many")))

(check-expect (donuts 4) "Number of donuts: 4")
(check-expect (donuts 9) "Number of donuts: 9")
(check-expect (donuts 10) "Number of donuts: many")
(check-expect (donuts 99) "Number of donuts: many")

;; B. both_ends
;; Given a string s, return a string made of the first 2
;; and the last 2 chars of the original string,
;; so 'spring' yields 'spng'. However, if the string length
;; is less than 2, return instead the empty string.

(define (both-ends s)
  (if (> 2 (string-length s))
      ""
      (let ((s1 (string->list s)))
        (list->string (append (take s1 2)
                              (take-right s1 2))))))


(check-expect (both-ends "spring") "spng")
(check-expect (both-ends "Hello") "Helo")
(check-expect (both-ends "a") "")
(check-expect (both-ends "xyz") "xyyz")

;; C. fix-start
;; Given a string s, return a string
;; where all occurences of its first char have
;; been changed to '*', except do not change
;; the first char itself.
;; e.g. 'babble' yields 'ba**le'
;; Assume that the string is length 1 or more.
;; Hint: s.replace(stra, strb) returns a version of string s
;; where all instances of stra have been replaced by strb.

(define (fix-start s)
  (let ((r (string-ref s 0)))
    (~a r (string-replace (substring s 1) (~a r) "*"))))

(check-expect (fix-start "babble") "ba**le")
(check-expect (fix-start "aardvark") "a*rdv*rk")
(check-expect (fix-start "google") "goo*le")
(check-expect (fix-start "donut") "donut")


;; D. MixUp
;; Given strings a and b, return a single string with a and b separated
;; by a space '<a> <b>', except swap the first 2 chars of each string.
;; e.g.
;;   'mix', pod' -> 'pox mid'
;;   'dog', 'dinner' -> 'dig donner'
;; Assume a and b are length 2 or more.
(define (split-at s n)
  (values (substring s 0 n)
          (substring s n)))

(define (mix-up s1 s2)
  (let-values (((a1 a2) (split-at s1 2))
               ((b1 b2) (split-at s2 2)))
    (~a b1 a2 " " a1 b2)))

  ;; (define (split s)
  ;;   (regexp-split #rx"^.{2}" s))
  ;; (string-join (map ~a (split s1) (split s2))))


(check-expect (mix-up "mix" "pod") "pox mid")
(check-expect (mix-up "dog" "dinner") "dig donner")
(check-expect (mix-up "gnash" "sport") "spash gnort")
(check-expect (mix-up "pezzy" "firm") "fizzy perm")



;; D. verbing
;; Given a string, if its length is at least 3,
;; add 'ing' to its end.
;; Unless it already ends in 'ing', in which case
;; add 'ly' instead.
;; If the string length is less than 3, leave it unchanged.
;; Return the resulting string.
(define (verbing s)
  (let ((len (string-length s)))
    (~a s (if (> len 3)
              (if (string=? "ing" (substring s (- len 3)  len))
                  "ly"
                  "ing")
              ""))))

(check-expect (verbing "hail") "hailing")
(check-expect (verbing "swiming") "swimingly")
(check-expect (verbing "do") "do")

;; E. not-bad
;; Given a string, find the first appearance of the
;; substring 'not' and 'bad'. If the 'bad' follows
;; the 'not', replace the whole 'not'...'bad' substring
;; with 'good'.
;; Return the resulting string.
;; So 'This dinner is not that bad!' yields:
;; This dinner is good!




(check-expect (not-bad "This movie is not so bad") "This movie is good")
(check-expect (not-bad "This dinner is not that bad!") "This dinner is good!")
(check-expect (not-bad "This tea is not hot") "This tea is not hot")
(check-expect (not-bad "It's bad yet not") "It's bad yet not")

;; F. front-back
;; Consider dividing a string into two halves.
;; If the length is even, the front and back halves are the same length.
;; If the length is odd, we'll say that the extra char goes in the front half.
;; e.g. 'abcde', the front half is 'abc', the back half 'de'.
;; Given 2 strings, a and b, return a string of the form
;;  a-front + b-front + a-back + b-back

(define (front-back s1 s2)
  (define (half s)
    (split-at s (ceiling (/ (string-length s) 2))))

  (let-values (((a1 a2) (half s1))
               ((b1 b2) (half s2)))
    (~a a1 b1 a2 b2)))


(check-expect (front-back "abcd" "xy") "abxcdy")
(check-expect (front-back "abcde" "xyz") "abcxydez")
(check-expect (front-back "Kitten" "Donut") "KitDontenut")


(test)
