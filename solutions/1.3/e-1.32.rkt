#lang sicp

(#%require rackunit)

;Exercise 1.32

;a) Show that sum and product (Exercise 1.31) are both special cases of a still
;more general notion called accumulate that combines a collection of terms,
;using some general accumulation function:
;
;(accumulate combiner null-value term a next b)
;
;accumulate takes as arguments the same term and range specifications as sum and
;product, together with a combiner procedure (of two arguments) that specifies
;how the current term is to be combined with the accumulation of the preceding
;terms and a null-value that specifies what base value to use when the terms
;run out. Write accumulate and show how sum and product can both be defined as
;simple calls to accumulate.
;
;b) If your accumulate procedure generates a recursive process, write one that
;generates an iterative process. If it generates an iterative process, write one
;that generates a recursive process.

;Solution


;Recursive version
(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner
                            null-value
                            term
                            (next a)
                            next
                            b))))


;Iterative version
(define (accumulate-iter combiner null-value term a next b)
  (define (iter x result)
    (if (> x b)
        result
        (iter (next x)
              (combiner result (term x)))))
  (iter a null-value))


(define (id x) x)
(define (inc x) (+ x 1))


(check-equal? (accumulate +
                          0
                          id
                          1
                          inc
                          5)
              15)
(check-equal? (accumulate +
                          2
                          id
                          1
                          inc
                          5)
              17)
(check-equal? (accumulate *
                          1
                          id
                          1
                          inc
                          5)
              120)
(check-equal? (accumulate *
                          2
                          id
                          1
                          inc
                          5)
              240)
(check-equal? (accumulate-iter +
                               0
                               id
                               1
                               inc
                               5)
              15)
(check-equal? (accumulate-iter +
                               2
                               id
                               1
                               inc
                               5)
              17)
(check-equal? (accumulate-iter *
                               1
                               id
                               1
                               inc
                               5)
              120)
(check-equal? (accumulate-iter *
                               2
                               id
                               1
                               inc
                               5)
              240)
