#lang sicp


(#%require rackunit)


;Exercise 1.12

;The following pattern of numbers is called Pascal’s triangle.
;                 1
;               1   1
;             1   2   1   
;           1   3   3   1
;         1   4   6   4   1
;       1   5  10   10  5   1
;                ...
;The numbers at the edge of the triangle are all 1, and each number inside the
;triangle is the sum of the two numbers above it. Write a procedure that
;computes elements of Pascal’s triangle by means of a recursive process.


;Solution

(define (c n k)
  (cond ((= k 0) 1)
        ((= n k) 1)
        (else (+ (c (- n 1) (- k 1))
                 (c (- n 1) k)))))


(check-equal? (c 0 0) 1)
(check-equal? (c 5 0) 1)
(check-equal? (c 6 6) 1)
(check-equal? (c 5 1) 5)
(check-equal? (c 5 2) 10)
(check-equal? (c 20 6) (c 20 14))



