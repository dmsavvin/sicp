#lang sicp

(#%require rackunit)

;Exercise 1.40

;Define a procedure cubic that can be used together with the newtons-method
;procedure in expressions of the form
;
;(newtons-method (cubic a b c) 1)
;
;to approximate zeros of the cubic x^3 + a*x^2 + b*x + c.

;Solution

(define tolerance 0.00001)
(define dx 0.00001)

(define (fixed-point f first-guess)
  (define (try guess)
    (let ([next (f guess)])
      (cond ((close-enough? guess next tolerance)
             next)
            (else
             (try next)))))
  (try first-guess))

(define (close-enough? v1 v2 t)
  (< (abs (- v1 v2))
     t))

(define (deriv g)
  (lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x ) (* b x) c)))


(check-true (close-enough? 2.0
                           (newtons-method (cubic 0.0 0.0 -8.0) 1.0)
                           0.0001))
(check-true (close-enough? -1.0
                           (newtons-method (cubic 3.0 3.0 1.0) 1.0)
                           0.0001))
(check-true (close-enough? 2.0
                           (newtons-method (cubic 0.0 -3.0 -2.0) 1.0)
                           0.0001))