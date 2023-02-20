#lang sicp

(#%require rackunit)


;Exercise 1.10

;The following procedure computes a mathematical function called Ackermann’s
;function.

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1) (A x (- y 1))))))

;What are the values of the following expressions?

;(A 1 10) ;1024
;(A 2 4) ;65536
;(A 3 3) ;65536

;Consider the following procedures, where A is the procedure defined above:

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
;(define (k n) (* 5 n n))

;Give concise mathematical definitions for the functions computed by the
;procedures f, g, and h for positive integer values of n. For example, (k n)
;computes 5*n^2


;Solution

;Mathematical definition for (A 0 n) is 2*n
;Mathematical definition for (A 1 n) is 2^n
;Mathematical definition for (A 2 n) is 2^(2^(2^(2^...n times... 

(check-equal? (A 1 10) (expt 2 10))
(check-equal? (A 2 4) (expt 2 (expt 2 (expt 2 2))))
(check-equal? (A 3 3) (expt 2 (expt 2 (expt 2 2))))

(check-equal? (f 1) (* 2 1))
(check-equal? (f 6) (* 2 6))
(check-equal? (f 11) (* 2 11))

(check-equal? (g 1) (expt 2 1))
(check-equal? (g 6) (expt 2 6))
(check-equal? (g 11) (expt 2 11))

(check-equal? (h 3) (expt 2 (expt 2 2)))
(check-equal? (h 5) (expt 2 (expt 2 (expt 2 (expt 2 2)))))
