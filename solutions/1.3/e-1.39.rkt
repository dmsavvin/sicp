#lang sicp

(#%require rackunit)

;Exercise 1.39

;A continued fraction representation of the tangent function was published in
;1770 by the German mathematician J.H. Lambert:
;  tan(x) = x / (1 - x^2 / (3 - x^2 / (5 - x^2 / ...
;where x is in radians. Define a procedure (tan-cf x k) that computes an
;approximation to the tangent function based on Lambert’s formula. k specifies
;the number of terms to compute, as in Exercise 1.37.

;Solution

;(define (cont-frac-rec n d k)
;  (define (rec k_)
;    (if (= k_ k)
;        (/ (n k_) (d k_))
;        (/ (n k_) (+ (d k_) (rec (+ k_ 1))))))
;  (rec 1))

(define (cont-frac-iter n d k)
  (define (iter k_ res)
    (if (= k_ 0)
        res
        (iter (- k_ 1) (/ (n k_) (+ (d k_) res)))))
  (iter k 0))

(define (n x)
  (lambda (i) (if (= i 1)
                  x
                  (- (* x x)))))
(define (d i) (- (* i 2) 1.0))

(define (tan-cf x k)
  (cont-frac-iter (n x) d k)); here we can use ither cont-frac-iter or cont-frac-rec

(define (close-enough? v1 v2 tolerance)
  (< (abs (- v1 v2)) tolerance))

(define tan-1 1.55740772465)
(define tan-2 (- 2.18503986326))

(check-true (close-enough? tan-1 (tan-cf 1 1000) 0.00001))
(check-true (close-enough? tan-2 (tan-cf 2 1000) 0.00001))