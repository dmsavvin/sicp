#lang sicp

(#%require rackunit)


;Exercise 1.11

;A function f is defined by the rule that

;f(n) = n if n < 3
;f(n) = f(n-1) + 2*f(n-2) + 3*f(n-3) if n >= 3

;Write a procedure that computes f by means of a recursive process. Write
;a procedure that computes f by means of an iterative process.


;Solution

;The procedure that computes f by means of a recursive process
(define (f-rec n)
  (define (fff x) (+ (f-rec (- x 1))
                     (* 2 (f-rec (- x 2)))
                     (* 3 (f-rec (- x 3)))))
  (if (< n 3) n
              (fff n)))

;The procedure that computes f by means of an iterative process
(define (f-iter m)
  (define (f-iter-h a b c counter n)
    (define new-a (+ (* 3 c) (* 2 b) a))
    (define new-counter (+ counter 1))
    (cond ((< n 3) n)
          ((< counter n) (f-iter-h new-a a b new-counter n))
          (else a)))
  (f-iter-h 2 1 0 2 m))


(check-equal? (f-rec 1) 1)
(check-equal? (f-rec 4) 11)
(check-equal? (f-rec 6) 59)

(check-equal? (f-iter 2) 2)
(check-equal? (f-iter 4) 11)
(check-equal? (f-iter 6) 59)