#lang sicp


(#%require rackunit)


;Exercise 1.16

;Design a procedure that evolves an iterative exponentiation process that uses
;successive squaring and uses a logarithmic number of steps, as does fast-expt.
;(Hint: Using the observation that (b^(n/2))^2 = (b^2)^(n/2), keep, along with
;the exponent n and the base b, an additional state variable a, and define the
;state transformation in such a way that the product a*b^n is unchanged from
;state to state. At the beginning of the process a is taken to be 1, and the
;answer is given by the value of a at the end of the process. In general, the
;technique of defining an invariant quantity that remains unchanged from state
;to state is a powerful way to think about the design of iterative algorithms.)

;Solution

(define (iter-fast-exp b-fe n-fe)
  (define (iter a b n); invariant a*b^n
    (cond ((= n 0) a)
          ((= (remainder n 2) 1)
           (iter (* a b) b (- n 1)))
          (else
           (iter a (* b b) (/ n 2)))))
  (iter 1 b-fe n-fe))


(check-equal? (iter-fast-exp 2 3) 8)
(check-equal? (iter-fast-exp 3 4) 81)
(check-equal? (iter-fast-exp 2 7) 128)
