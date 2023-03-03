#lang sicp

(#%require rackunit)

;Exercise 1.23

;The smallest-divisor procedure shown at the start of this section does lots of
;needless testing: After it checks to see if the number is divisible by 2 there
;is no point in checking to see if it is divisible by any larger even numbers.
;This suggests that the values used for test-divisor should not be 2, 3, 4, 5,
;6, ..., but rather 2, 3, 5, 7, 9, .... To implement this change, define a
;procedure next that returns 3 if its input is equal to 2 and otherwise returns
;its input plus 2. Modify the smallest-divisor procedure to use (next test-
;divisor) instead of (+ test-divisor 1). With timed-prime-test incorporating
;this modified version of smallest-divisor, run the test for each of the 12
;primes found in Exercise 1.22. Since this modification halves the number of
;test steps, you should expect it to run about twice as fast. Is this
;expectation confirmed? If not, what is the observed ratio of the speeds of the
;two algorithms, and how do you explain the fact that it is different from 2?

;Solution

(define (square x) (* x x))

(define (divides? a b) (= (remainder b a) 0))

(define (next x) (if (= x 2) 3 (+ x 2)))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (n-find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (n-find-divisor n (next test-divisor)))))

(define (smallest-divisor n) (n-find-divisor n 2))

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

(search-for-prime 1000000 1000038)
(search-for-prime 10000000 10000104)
(search-for-prime 100000000 100000048)
(search-for-prime 1000000000 1000000022)
(search-for-prime 10000000000 10000000062)
(search-for-prime 100000000000 100000000058)

;Using 'next' procedure it is possible to reduce time of the primility testing
;by a factor of 1.3-1.4 which is less than the expected 2. The reason is that
;there is an overhead associated with the 'next' procedure: external procedure
;call (next itself), (if ...) evaluation, (= x 2) evaluation.