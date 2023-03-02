#lang sicp

(#%require rackunit)

;Exercise 1.22

;Most Lisp implementations include a primitive called runtime that returns an
;integer that specifies the amount of time the system has been running
;(measured, for example, in microseconds). The following timedprime-test
;procedure, when called with an integern, prints n and checks to see if n is
;prime. If n is prime, the procedure prints three asterisks followed by the
;amount of time used in performing the test.

;(define (timed-prime-test n)
;  (newline)
;  (display n)
;  (start-prime-test n (runtime)))
;(define (start-prime-test n start-time)
;  (if (prime? n)
;      (report-prime (- (runtime) start-time))))
;(define (report-prime elapsed-time)
;  (display " *** ")
;  (display elapsed-time))

;Using this procedure, write a procedure search-for-primes that checks the
;primality of consecutive odd integers in a specified range. Use your procedure
;to find the three smallest primes larger than 1000; larger than 10,000; larger
;than 100,000; larger than 1,000,000. Note the time needed to test each prime.
;Since the testing algorithm has order of growth of Θ(√n), you should expect
;that testing for primes around 10,000 should take about √10 times as long as
;testing for primes around 1000. Do your timing data bear this out? How well do
;the data for 100,000 and 1,000,000 support the Θ(√n) prediction? Is your result
;compatible with the notion that programs on your machine run in time
;proportional to the number of steps required for the computation?

;Solution

(define (square x) (* x x))

(define (smallest-divisor n) (find-divisor n 2))

(define (divides? a b) (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
;  (newline)
;  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
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
  (if (divides? 2 begin) (iter (+ begin 1)) (iter begin)))

;(search-for-prime 1000000 1000038)
;(search-for-prime 10000000 10000104)
;(search-for-prime 100000000 100000048)
;(search-for-prime 1000000000 1000000022)
;(search-for-prime 10000000000 10000000062)
;(search-for-prime 100000000000 100000000058)

;| Starting value | Average duration | Current duration / |
;|                |                  | previous duration  |
;+----------------+------------------+--------------------+
;| 1000000        |      40.3        |                    |
;| 10000000       |     121.7        |      3.02          |
;| 100000000      |     427.0        |      3.51          |
;| 1000000000     |    1484.3        |      3.48          |
;| 10000000000    |    6394.3        |      4.31          |
;| 100000000000   |   17047.7        |      2.67          |

;As tested numbers grow by the factor of 10 time required for testing grows by
;the factor of ~3.4 (on average) which is quiet close to the theoretically
;predicted √10 which is ~3.16


