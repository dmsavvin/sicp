#lang sicp

(#%require rackunit)

;Exercise 1.3

;Define a procedure that takes three numbers
;as arguments and returns the sum of the squares of the two
;larger numbers.

(define (sqsum a b) (+ (* a a) (* b b)))
(define (>= a b) (or (> a b)
                     (= a b)))
(define (minl a b c) (and (>= a c)
                          (>= b c)))
(define (max2sqsum a b c)
  (cond ((minl a b c) (sqsum a b))
        ((minl c a b) (sqsum c a))
        (else (sqsum b c))))


(check-equal? (max2sqsum 1 2 3) 13)
(check-equal? (max2sqsum 2 1 3) 13)
(check-equal? (max2sqsum 3 1 2) 13)
(check-equal? (max2sqsum 1 3 2) 13)
(check-equal? (max2sqsum 2 3 1) 13)
(check-equal? (max2sqsum 3 2 1) 13)
(check-equal? (max2sqsum 1 1 2) 5)
(check-equal? (max2sqsum 1 2 1) 5)
(check-equal? (max2sqsum 2 1 1) 5)
(check-equal? (max2sqsum 2 2 1) 8)
(check-equal? (max2sqsum 2 1 2) 8)
(check-equal? (max2sqsum 1 2 2) 8)
(check-equal? (max2sqsum 3 3 3) 18)