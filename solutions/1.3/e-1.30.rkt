#lang sicp

(#%require rackunit)

;Exercise 1.30

;The sum procedure above generates a linear recursion. The procedure can be
;rewritten so that the sum is performed iteratively. Show how to do this by
;filling in the missing expressions in the following definition:

;(define (sum term a next b)
;  (define (iter a result)
;    (if ⟨??⟩
;        ⟨??⟩
;        (iter ⟨??⟩ ⟨??⟩)))
;  (iter ⟨??⟩ ⟨??⟩))


;Solution

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (cube x) (* x x x))
(define (square x) (* x x))
(define (inc x) (+ x 1))


(check-equal? (sum cube 1 inc 3) (sum-iter cube 1 inc 3))
(check-equal? (sum square 1 inc 3) (sum-iter square 1 inc 3))