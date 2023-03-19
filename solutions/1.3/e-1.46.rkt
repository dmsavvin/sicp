#lang sicp

(#%require rackunit)

;Exercise 1.46

;Several of the numerical methods described in this chapter are instances of an
;extremely general computational strategy known as iterative improvement.
;Iterative improvement says that, to compute something, we start with an
;initial guess for the answer, test if the guess is good enough, and otherwise
;improve the guess and continue the process using the improved guess as the new
;guess. Write a procedure iterative-improve that takes two procedures as
;arguments: a method for telling whether a guess is good enough and a method
;for improving a guess. iterative-improve should return as its value a
;procedure that takes a guess as argument and keeps improving the guess until
;it is good enough. Rewrite the sqrt procedure of Section 1.1.7 and the fixed-
;point procedure of Section 1.3.3 in terms of iterative-improve.

;Solution

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess))))
  (lambda (guess) (iter guess)))

(define (close-enough? v1 v2 t)
  (< (abs (- v1 v2))
     t))
(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (good-enough? guess)
    (close-enough? guess (f guess) tolerance))
  ((iterative-improve good-enough? f) first-guess))

(define (sqrt x)
  (define (improve guess)
    (/ (+ guess (/ x guess)) 2.0))
  (define (good-enough? guess)
    (close-enough? guess (improve guess) tolerance))
  ((iterative-improve good-enough? improve) 1))

(define test-f
  (lambda (x) (/ (+ (/ 16 x) x) 2)))


(check-true (close-enough? 4
                           (fixed-point test-f 3)
                           tolerance))
(check-true (close-enough? 4
                           (sqrt 16)
                           tolerance))
(check-true (close-enough? 5
                           (sqrt 25)
                           tolerance))