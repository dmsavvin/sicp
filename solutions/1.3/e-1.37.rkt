#lang sicp

(#%require rackunit)

;Exercise 1.37

;a) An infinite continued fraction is an expression of the form
; f = N1 / (D1 + N2 / (D2 + N3 / (D3 + ...
;As an example, one can show that the infinite continued fraction expansion with
;the Ni and the Di all equal to 1 produces 1/φ, where φ is the golden ratio
;(described in Section 1.2.2). One way to approximate an infinite continued
;fraction is to truncate the expansion after a given number of terms. Such a
;truncation — a so-called k-term finite continued fraction — has the form
; N1 / (D1 + N2 / (D2 + N3 / ( ... / (Dk-1 + Nk / Dk) ...)))
;Suppose that n and d are procedures of one argument (the term index i) that
;return the Ni and Di of the terms of the continued fraction. Define a procedure
;cont-frac such that evaluating (cont-frac n d k) computes the value of the
;k-term finite continued fraction. Check your procedure by approximating 1/φ
;using
;
;(cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           k)
;
;for successive values of k. How large must you make k in order to get an
;approximation that is accurate to 4 decimal places?
;
;b) If your cont-frac procedure generates a recursive process, write one that
;generates an iterative process. If it generates an iterative process, write one
;that generates a recursive process.

;Solution

(define (cont-frac-rec n d k) ;a)
  (define (rec k_)
    (if (= k_ k)
        (/ (n k_) (d k_))
        (/ (n k_) (+ (d k_) (rec (+ k_ 1))))))
  (rec 1))

(define (cont-frac-iter n d k) ;b)
  (define (iter k_ res)
    (if (= k_ 0)
        res
        (iter (- k_ 1) (/ (n k_) (+ (d k_) res)))))
  (iter k 0))

(define (n-test i) 1)
(define (d-test i) 1)

(define (n i) 1.0)
(define (d i) 1.0)

(define (close-enough? v1 v2 tolerance)
  (< (abs (- v1 v2)) tolerance))

(define φ 1.618033988749)

(check-equal? (cont-frac-rec n-test d-test 1) 1)
(check-equal? (cont-frac-rec n-test d-test 4) (/ 3 5))

(check-equal? (cont-frac-iter n-test d-test 1) 1)
(check-equal? (cont-frac-iter n-test d-test 4) (/ 3 5))

(check-true (close-enough? φ (/ 1 (cont-frac-rec n d 1000)) 0.00001))
(check-true (close-enough? φ (/ 1 (cont-frac-iter n d 1000)) 0.00001))