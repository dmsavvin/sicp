#lang sicp

(#%require rackunit)

;Exercise 1.42

;Let f and д be two one-argument functions. The composition f aer g is defined
;to be the function x 7 → f(g(x)). Define a procedure compose that implements
;composition. For example, if inc is a procedure that adds 1 to its argument,
;
;((compose square inc) 6)
;49

;Solution

(define (compose f g)
  (lambda (x) (f (g x))))

(define (inc x) (+ x 1))
(define (square x) (* x x))


(check-equal? ((compose square inc) 6) 49)