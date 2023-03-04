#lang sicp

(#%require rackunit)

;Exercise 1.24

;Modify the timed-prime-test procedure of Exercise 1.22 to use fast-prime?
;(the Fermat method), and test each of the 12 primes you found in that
;exercise. Since the Fermat test has Θ(log n) growth, how would you expect the
;time to test primes near 1,000,000 to compare with the time needed to test
;primes near 1000? Do your data bear this out? Can you explain any discrepancy
;you find?

;Solution

(define (square x) (* x x))

(define (even? x) (= (remainder x 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


;(define (divides? a b) (= (remainder b a) 0))

;(define (next x) (if (= x 2) 3 (+ x 2)))

;(define (find-divisor n test-divisor)
;  (cond ((> (square test-divisor) n) n)
;        ((divides? test-divisor n) test-divisor)
;        (else (find-divisor n (+ test-divisor 1)))))

;(define (n-find-divisor n test-divisor)
;  (cond ((> (square test-divisor) n) n)
;        ((divides? test-divisor n) test-divisor)
;        (else (n-find-divisor n (next test-divisor)))))

;(define (smallest-divisor n) (n-find-divisor n 2))

;(define (prime? n)
;  (= n (smallest-divisor n)))

(define (timed-prime-test n)
;  (newline)
;  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (fast-prime? n 10)
      (report-prime (- (runtime) start-time) n)
      (values)))

(define (report-prime elapsed-time n)
  (newline)
  (display n)
  (display " *** ")
  (display elapsed-time))

(define (search-for-prime begin end)
  (define (iter n)
    (cond ((<= n end) (timed-prime-test n) (iter (+ n 2)))
          (else (newline) (display "done"))))
  (if (even? begin) (iter (+ begin 1)) (iter begin)))

(search-for-prime 10000 10038)
(search-for-prime 100000 100045)
(search-for-prime 1000000 1000038)
(search-for-prime 10000000 10000110)
(search-for-prime 100000000 100000040)
(search-for-prime 1000000000 1000000025)

;In the table below there are average times needed to test the primes of a
;particular size
;
;| Size           | Average time to  |
;|                | test a primality |
;+----------------+------------------+
;| 10000          |   13             |
;| 100000         |   19.7           |
;| 1000000        |   23             |
;| 10000000       |   26             |
;| 100000000      |   28             |
;| 1000000000     |   35             |
;
;Since algorithm has Θ(log n) growth, for every doubling of the number of
;zeroes in the input we expect doubling the testing time. In particular as
;testing time for 10000 is 13 (as in the table above) than for 100000000 we
;would expect testing time to be 26. Actually we have 28 which is quite close.