#lang sicp

(#%require rackunit)


;Exercise 1.7

;The good-enough? test used in computing square roots will not be very effective
;for finding the square roots of very small numbers. Also, in real computers,
;arithmetic operations are almost always performed with limited precision. This
;makes our test inadequate for very large numbers. Explain these statements,
;with examples showing how the test fails for small and large numbers. An
;alternative strategy for implementing good-enough? is to watch how guess
;changes from one iteration to the next and to stop when the change is a very
;small fraction of the guess. Design a square-root procedure that uses this kind
;of end test. Does this work better for small and large numbers?

;Solution

;Suppose we find the sqrt of the very small number x_^2 using original procedure.
;For the found result guess (assuming guess > x_) guess^2 - x_2 = d where d is
;some very small positive number. So guess - x_ = d / (guess + x_). For the very
;small guess and x_ difference guess - x_ can be significantly greater than d.
;In particular if we require precision to be 0.001 and since 0.03^2 = 0.0009 <
;0.001 than 0.03 will be the valid result for the sqrt of any number from 0 to
;0.0009. For example for (sqrt 0.0001) the result is 0.032308... which is more
;then x3 of the expected result.

;There are only finite amount of the floating point numbers in any given interval
;of the R. The further from the 0 the the more sparse floating point numbers
;become. If the exact result of the arithmetic operation with the floating
;numbers lies between two adjacent floating point numbers then the nearest
;floating point number is treated as a result. So if there is some f(x) and
;f(x) is close enough to x then for floating point number (especially for the
;big one) it is possible for f(x) == x to be true. In our case if we are in the
;situation where guess is big, close to the solution but should be improved it
;is possible that improvement will be so small that (improve guess x) will be
;evaluated to the same guess which will cause the infinite loop for the
;sqrt-iter. For example for the (sqrt 1123456789987654321212121111111111)
;sqrt-iter enter an endless cycle for the guess = 33518006951303864 where amount
;of improvements is expected to be -2.81... and the distance between adjacent
;floating point numbers is 4.

;Implementing new version of good-enough? procedure based on the rule of stopping
;iterations when changes of the guess become small enough we significantly
;increase the accuracy for the small arguments and fix the infinite loop error for
;the big arguments

(define (square x) (* x x))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (new-good-enough? guess next-guess)
  (< (/ (abs (- guess next-guess)) guess) 0.00000000001))
  ;(= guess next-guess))

(define (average x y)
  (/ (+ x y) 2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter guess x)
  (if (new-good-enough? guess (improve guess x))
    guess
    (sqrt-iter (improve guess x) x)))

(define (sqrt x) (sqrt-iter 1.0 x))


(define big-value 1123456789987654321212121111111111)
(define small-value 0.0001)

(define big-value-sqrt (sqrt big-value))
(define small-value-sqrt (sqrt small-value))


(check-true (new-good-enough? big-value-sqrt
                              (improve big-value-sqrt big-value)))
(check-true (new-good-enough? small-value-sqrt
                              (improve small-value-sqrt small-value)))
