#lang sicp

;Exercise 1.5

;Ben Bitdiddle has invented a test to determine whether the interpreter he is
;faced with is using applicativeorder evaluation or normal-order evaluation.
;He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0) 0 y))

;Then he evaluates the expression

(test 0 (p))

;What behavior will Ben observe with an interpreter that uses applicative-order
;evaluation? What behavior will he observe with an interpreter that uses normal-
;order evaluation? Explain your answer. (Assume that the evaluation rule for the
;special form if is the same whether the interpreter is using normal or
;applicative order: The predicate expression is evaluated first, and the result
;determines whether to evaluate the consequent or the alternative expression.)

;Solution

;The interpreter that uses applicative-oder evaluation in order to evaluate
;combination (text 0 (p)) should first evaluate each subexpression (operator and
;operands) of the combination. In particular it should evaluate (p) that ends up
;with infinit sycle.

;In the case of the normal-order evaluation interpreter evaluation will start
;with evaluation of the test expression to (if (= x 0) x y). Then predicate will
;be evaluated to #t (x will be substituted for 0). Since predicate is evaluated
;to #t consequent expression x will be chosen and will be evaluated to 0. So in
;the case of the normal-order evaluation interpreter combination (test 0 (p))
;will be evaluated to 0.
