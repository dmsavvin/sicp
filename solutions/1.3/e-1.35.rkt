#lang sicp

(#%require rackunit)

;Exercise 1.35

;Show that the golden ratio ϕ (Section 1.2.2) is a fixed point of the
;transformation x → 1 + 1/x, and use this fact to compute ϕ by means of the
;fixed-point procedure.

;Solution

;If x is a fixed point of the transformation x → 1 + 1/x then x = 1 + 1/x =>
;x^2 - x - 1 = 0. ϕ is a solution for this equation hence ϕ is a fixed point for
;the transformation x → 1 + 1/x.

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next tolerance)
          next
          (try next))))
  (try first-guess))

(define (close-enough? v1 v2 t)
  (< (abs (- v1 v2))
     t))

(define φ (/ (+ 1 (sqrt 5)) 2))
(define gr (lambda (x) (+ 1 (/ 1 x))))


(check-true (close-enough? φ (fixed-point gr 1.0) tolerance))
