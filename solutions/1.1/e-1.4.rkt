#lang sicp

(#%require rackunit)

;Exercise 1.4

;Observe that our model of evaluation allows
;for combinations whose operators are compound
;expressions. Use this observation to describe
;the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;Expression (if (> b 0) + -) evaluates to +
;(the primitiv procedure) if (> b 0) evaluates
;to #t otherwise it evaluates to -. Then the
;result (+ or -) is applied to a and b so if
;(> b 0) evaluates to #t we get (+ a b), if
;(> b 0) evaluates to #f we get (- a b).

(check-equal? (a-plus-abs-b 2 3) 5)
(check-equal? (a-plus-abs-b 2 (- 3)) 5)
(check-equal? (a-plus-abs-b (- 12) 3) (- 9))
(check-equal? (a-plus-abs-b (- 12) (- 3)) (- 9))