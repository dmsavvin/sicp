#lang sicp

(#%require rackunit)

;Exercise 1.1

;Below is a sequence of expressions. What is the result printed by the
;interpreter in response to each expression?

(define q1 10) ;10
(define q2 (+ 5 3 4)) ;12
(define q3 (- 9 1)) ;8
(define q4 (/ 6 2)) ;3
(define q5 (+ (* 2 4) (- 4 6))) ;6

(define a 3)
(define b (+ a 1)) ;b=4

(define q6 (+ a b (* a b))) ;19
(define q7 (= a b)) ; #f
(define q8 (if (and (> b a) (< b (* a b)))
               b
               a)) ;4 (b > a and b < a * b <=> 4 > 3 and 4 < 3 * 4)
(define q9 (cond ((= a 4) 6)
                 ((= b 4) (+ 6 7 a))
                 (else 25))) ;16
(define q10 (+ 2 (if (> b a) b a))) ;6
(define q11 (* (cond ((> a b) a)
                     ((< a b) b)
                     (else -1))

               (+ a 1))) ;16

(check-equal? q1 10)
(check-equal? q2 12)
(check-equal? q3 8)
(check-equal? q4 3)
(check-equal? q5 6)
(check-equal? q6 19)
(check-equal? q7 #f)
(check-equal? q8 4)
(check-equal? q9 16)
(check-equal? q10 6)
(check-equal? q11 16)
