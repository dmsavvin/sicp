#lang sicp

(#%require rackunit)

;Exercise 1.38

;In 1737, the Swiss mathematician Leonhard Euler published a memoir De
;Fractionibus Continuis, which included a continued fraction expansion for e-2,
;where e is the base of the natural logarithms. In this fraction, the Ni are
;all 1, and the Di are successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, ... Write a
;program that uses your cont-frac procedure from Exercise 1.37 to approximate e,
;based on Euler’s expansion.

;Solution

(define (cont-frac-rec n d k)
  (define (rec k_)
    (if (= k_ k)
        (/ (n k_) (d k_))
        (/ (n k_) (+ (d k_) (rec (+ k_ 1))))))
  (rec 1))

(define (cont-frac-iter n d k)
  (define (iter k_ res)
    (if (= k_ 0)
        res
        (iter (- k_ 1) (/ (n k_) (+ (d k_) res)))))
  (iter k 0))

(define (n-test i) (if (> i 0) 1 1))
(define (d-test i)
  (if (= (remainder i 3) 2)
      (* (+ (/ (- i 2) 3) 1) 2)
      1))

(define (n i) (if (> i 0) 1.0 1.0))
(define (d i)
  (if (= (remainder i 3) 2)
      (* (+ (/ (- i 2) 3) 1) 2)
      1))

(define (close-enough? v1 v2 tolerance)
  (< (abs (- v1 v2)) tolerance))

(define e 2.718281828459045)

(check-equal? (cont-frac-rec n-test d-test 1) 1)
(check-equal? (cont-frac-rec n-test d-test 5) (/ 23 32))

(check-equal? (cont-frac-iter n-test d-test 1) 1)
(check-equal? (cont-frac-iter n-test d-test 5) (/ 23 32))

(check-true (close-enough? e (+ (cont-frac-rec n d 1000) 2) 0.00001))
(check-true (close-enough? e (+ (cont-frac-iter n d 1000) 2) 0.00001))