#lang sicp


(#%require rackunit)


;Exercise 1.18

;Using the results of Exercise 1.16 and Exercise 1.17, devise a procedure that
;generates an iterative process for multiplying two integers in terms of adding,
;doubling, and halving and uses a logarithmic number of steps

;Solution

(define (double n) (* n 2))

(define (halve n) (/ n 2))

(define (iter-fast* a* b*)
  (define (iter res a b); invariant is res+a*b
    (cond ((= b 0) res)
          ((= (remainder b 2) 0)
           (iter res
                 (double a)
                 (halve b)))
          (else
           (iter (+ res a)
                 (double a)
                 (halve (- b 1))))))
  (iter 0 a* b*))


(check-equal? (iter-fast* 2 3) 6)
(check-equal? (iter-fast* 3 4) 12)
(check-equal? (iter-fast* 2 7) 14)
(check-equal? (iter-fast* 31 11) 341)
