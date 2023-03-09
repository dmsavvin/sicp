#lang sicp

(#%require rackunit)

;Exercise 1.33

;You can obtain an even more general version of accumulate (Exercise 1.32) by
;introducing the notion of a filter on the terms to be combined. That is,
;combine only those terms derived from values in the range that satisfy a
;specified condition.The resulting filtered-accumulate abstraction takes the same
;arguments as accumulate, together with an additional predicate of one argument
;that specifies the filter. Write filtered-accumulate as a procedure. Show how
;to express the following using filteredaccumulate:

;a) the sum of the squares of the prime numbers in the interval a to b (assuming
;that you have a prime? predicate already written)

;b) the product of all the positive integers less than n that are relatively
;prime to n (i.e., all positive integers i < n such that GCD(i, n) = 1).

;Solution

(define (accumulate combiner filter null-value term a next b)
  (define (iter x result)
    (let ([candidate (term x)])
    (if (> x b)
        result
        (iter (next x)
              (if (filter candidate)
                  (combiner result candidate)
                  result)))))
  (iter a null-value))

;a)
(define (sum-of-squared-primes a b)
  (define (add-square x y) (+ x (square y)))
  (accumulate add-square prime? 0 id a inc b))

;b)
(define (sum-of-relatively-prime n)
  (define (mutually-prime? x) (= (gcd x n) 1))
  (accumulate * mutually-prime? 1 id 1 inc n))

(define (even? x) (= (remainder x 2) 0))
(define (id x) x)
(define (square x) (* x x))
(define (inc x) (+ x 1))


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

;Testing for primality
(define (expmod base exp m)
  (define (one x)
    (define res (remainder (square x) m))
    (if (and (not (= x 1))
             (not (= x (- m 1)))
             (= res 1))
        0
        res))
  (cond ((= exp 0) 1)
        ((even? exp)
         (one (expmod base (/ exp 2) m)))
        (else
          (remainder
           (* base (expmod base (- exp 1) m))
           m))))

(define (mr-test? n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (if (= n 1)
      #f
      (try-it (max 1 (random n))))) ;a is chosen randomly from [1, n-1]
 
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((mr-test? n) (fast-prime? n (- times 1)))
        (else false)))

(define (prime? x)
  (fast-prime? x 50))


(check-equal? (accumulate + even? 0 id 1 inc 10) 30)
(check-equal? (accumulate * even? 1 id 1 inc 5) 8)
(check-equal? (accumulate + even? 3 id 1 inc 10) 33)
(check-equal? (accumulate * even? 2 id 1 inc 5) 16)

;a)

(check-equal? (sum-of-squared-primes 3 6) 34)
(check-equal? (sum-of-squared-primes 1 2) 4)
(check-equal? (sum-of-squared-primes 8 10) 0)

;b)
(check-equal? (sum-of-relatively-prime 1) 1)
(check-equal? (sum-of-relatively-prime 2) 1)
(check-equal? (sum-of-relatively-prime 5) 24)
(check-equal? (sum-of-relatively-prime 6) 5)