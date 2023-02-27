#lang sicp


(#%require rackunit)


;Exercise 1.19

;There is a clever algorithm for computing the Fibonacci numbers in a
;logarithmic number of steps. Recall the transformation of the state variables
;a and b in the fib-iter process of Section 1.2.2: a <-- a + b and b <-- a. Call
;this transformation T , and observe that applying T over and over again n
;times, starting with 1 and 0, produces the pair Fib(n + 1) and Fib(n). In other
;words, the Fibonacci numbers are produced by applying T^n, the n^th power of
;the transformation T, starting with the pair (1, 0). Now consider T to be the
;special case of p = 0 and q = 1 in a family of transformations T_pq , where
;T_pq transforms the pair (a, b) according to a <-- bq + aq + ap and b <-- bp +
;aq. Show that if we apply such a transformation Tpq twice, the effect is the
;same as using a single transformation T_p′q′ of the same form, and compute p′
;and q′ in terms of p and q. This gives us an explicit way to square these
;transformations, and thus we can compute T^n using successive squaring, as in
;the fast-expt procedure. Put this all together to complete the following
;procedure, which runs in a logarithmic number of steps:

;(define (fib n)
;  (fib-iter 1 0 0 1 n))
;(define (fib-iter a b p q count)
;  (cond ((= count 0) b)
;        ((even? count)
;         (fib-iter a
;                   b
;                   ⟨??⟩ ; compute p′
;                   ⟨??⟩ ; compute q′
;                   (/ count 2)))
;        (else (fib-iter (+ (* b q) (* a q) (* a p))
;                        (+ (* b p) (* a q))
;                        p
;                        q
;                        (- count 1)))))

;Solution

;T_pq is a linear trasformaton with the matrix
;
;  | p+q q |
;  |       |
;  | q   p |
;
;T_pq * T_pq is a linear tranformation with the matrix
;
;  | p+q q |   | p+q q |   | (p+q)^2+q^2 q*(2p+q) |
;  |       | * |       | = |                      |
;  | q   p |   | q   p |   | q*(2p+q)    p^2+q^2  |
;
;which is T_p'q' with p' = q^2 + p^2, q' = q * (2*p + q) (it is obvious that
;p'+q' = (p+q)^2+q^2)

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (define (p_ _p _q)
    (+ (* _p _p) (* _q _q)))
  (define (q_ _p _q)
    (* _q (+ (* 2 _p) _q)))
  (define (even? x) (= (remainder x 2) 0))
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (p_ p q) ; compute p′
                   (q_ p q) ; compute q′
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))




(check-equal? (fib 0) 0)
(check-equal? (fib 1) 1)
(check-equal? (fib 2) 1)
(check-equal? (fib 3) 2)
(check-equal? (fib 8) 21)
