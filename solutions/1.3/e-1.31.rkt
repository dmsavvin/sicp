#lang sicp

(#%require rackunit)

;Exercise 1.31

;a) The sum procedure is only the simplest of a vast number of similar
;abstractions that can be captured as higher-order procedures. Write an
;analogous procedure called product that returns the product of the values of a
;function at points over a given range. Show how to define factorial in terms of
;product. Also use product to compute approximations to π using the formula
;
;      pi   2*4*4*6*6*8...
;      -- = ---------------
;      4    3*3*5*5*7*7...
;
;b) If your product procedure generates a recursive process, write one that
;generates an iterative process. If it generates an iterative process, write one
;that generates a recursive process.

;Solution

;Linear reqursion
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
          (product term (next a) next b))))

;Iteration
(define (product-iter term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x) (* (term x) result))))
  (iter a 1))

(define (factorial x)
  (if (= x 0)
      1
      (product id 1 inc x)))

(define (pi n);n should be even
  (* 2
     n
     (/ (product square 2 double-inc (- n 2))
        (product square 3 double-inc (- n 1)))))

(define (pi-alt n)
  (* (product term-pi 1 inc n)
     2))

(define (id x) x)
(define (inc x) (+ x 1))
(define (square x) (* x x))
(define (double-inc x) (+ x 2))
(define (term-pi x)
  (define double-x (* 2 x))
  (* (/ double-x (- double-x 1))
     (/ double-x (+ double-x 1))))


(check-equal? (product id 1 inc 5) 120)
(check-equal? (product-iter id 1 inc 5) 120)
(check-equal? (product id 0 inc 0) 0)
(check-equal? (product-iter id 0 inc 0) 0)
(check-equal? (product id 1 inc 1) 1)
(check-equal? (product-iter id 1 inc 1) 1)
(check-equal? (product id 5 inc 5) 5)
(check-equal? (product-iter id 5 inc 5) 5)
(check-equal? (product square 1 inc 3) 36)
(check-equal? (product-iter square 1 inc 3) 36)

(check-equal? (factorial 5) 120)
(check-equal? (factorial 0) 1)
(check-equal? (factorial 1) 1)
(check-equal? (factorial 3) 6)

(check-equal? (pi 8) (/ 36864 11025))
(check-equal? (pi 4) (/ 32 9))

(check-equal? (pi-alt 3) (/ 4608 1575))