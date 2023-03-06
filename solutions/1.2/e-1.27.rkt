#lang sicp

(#%require rackunit)

;Exercise 1.27

;Demonstrate that the Carmichael numbers listed in Footnote 1.47 really do fool
;the Fermat test. That is, write a procedure that takes an integer n and tests
;whether a^n is congruent to a modulo n for every a < n, and try your procedure
;on the given Carmichael numbers.

;Solution

(define (square x) (* x x))

(define (even? x) (= (remainder x 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test a n)
  (= (expmod a n n) a))

(define (fermat-is-fooled? n)
  (define (iter a)
    (cond ((= a n) #t)
          ((fermat-test a n) (iter (inc a)))
          (else #f)))
  (iter 2))

;Carmichael numbers
(check-true (fermat-is-fooled? 561))
(check-true (fermat-is-fooled? 1105))
(check-true (fermat-is-fooled? 1729))
(check-true (fermat-is-fooled? 2465))
(check-true (fermat-is-fooled? 2821))
(check-true (fermat-is-fooled? 6601))

;Primes
(check-true (fermat-is-fooled? 13))
(check-true (fermat-is-fooled? 839))
(check-true (fermat-is-fooled? 1009))
(check-true (fermat-is-fooled? 1451))

;Composit numbers
(check-true (not (fermat-is-fooled? 4)))
(check-true (not (fermat-is-fooled? 100)))
(check-true (not (fermat-is-fooled? 121)))
(check-true (not (fermat-is-fooled? 529)))