#lang sicp

(#%require rackunit)

;Exercise 1.43

;If f is a numerical function and n is a positive integer, then we can form the
;n-th repeated application of f , which is defined to be the function whose
;value at x is f (f (...(f (x))...)). For example, if f is the function x → x +
;1, then the n-th repeated application of f is the function x → x + n. If f is
;the operation of squaring a number, then the n-th repeated application of f is
;the function that raises its argument to the 2n-th power. Write a procedure
;that takes as inputs a procedure that computes f and a positive integer n and
;returns the procedure that computes the n-th repeated application of f. Your
;procedure should be able to be used as follows:
;
;((repeated square 2) 5)
;625
;
;Hint: You may find it convenient to use compose from Exercise 1.42.

;Solution

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define id (lambda (x) x))
  (define (odd? x) (= (remainder x 2) 1))
  (define (iter res n_)
    (cond ((= n_ 0) res)
          ((odd? n_) (iter (compose f res)
                           (- n_ 1)))
          (else (iter (compose res res)
                      (/ n_ 2)))))
  (if (= n 0)
      id
      (iter f (- n 1))))

(define (square x ) (* x x))

(check-equal? ((repeated square 2) 5) 625)
(check-equal? ((repeated square 3) 5) (* 625 625))
(check-equal? ((repeated square 0) 5) 5)
    