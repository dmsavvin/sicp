#lang sicp

(#%require rackunit)

;Exercise 1.8

;Newton’s method for cube roots is based on the fact that if y is an
;approximation to the cube root of x, then a beer approximation is given by
;the value (x / y^2 + 2*y) / 3. Use this formula to implement a cube-root
;procedure analogous to the square-root procedure.

;Solution

;(define (cube x) (* x x x))

(define (good-enough? guess next-guess)
  (< (/ (abs (- guess next-guess)) guess) 0.00000000001))

(define (improve guess x)
  (/ (+ (/ x (* guess guess))
        (* 2 guess))
     3))

(define (crt-iter guess x)
  (if (good-enough? guess (improve guess x))
    guess
    (crt-iter (improve guess x) x)))

(define (crt x) (crt-iter 1.0 x))


(define big-value 1123456789987654321212121111111111)
(define small-value 0.000001)

(define big-value-crt (crt big-value))
(define small-value-crt (crt small-value))

(check-true (good-enough? big-value-crt
                          (improve big-value-crt big-value)))
(check-true (good-enough? small-value-crt
                          (improve small-value-crt small-value)))
