#lang sicp

(#%require rackunit)

;Exercise 1.45

;We saw in Section 1.3.3 that attempting to compute square roots by naively
;finding a fixed point of y → x/y does not converge, and that this can be fixed
;by average damping. the same method works for finding cube roots as fixed
;points of the average-damped y → x/y^2. Unfortunately, the process does not
;work for fourth roots — a single average damp is not enough to make a fixed-
;point search for y → x/y^3 converge. On the other hand, if we average damp
;twice (i.e., use the average damp of the average damp of y → x/y^3) the fixed-
;point search does converge. Do some experiments to determine how many average
;damps are required to compute n-th roots as a fixedpoint search based upon
;repeated average damping of y → x/y^{n−1}. Use this to implement a simple
;procedure for computing n-th roots using fixed-point, average-damp, and the
;repeated procedure of Exercise 1.43. Assume that any arithmetic operations you
;need are available as primitives.

;Solution

(define tolerance 0.00001)
(define (close-enough? v1 v2 t)
  (< (abs (- v1 v2))
     t))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ([next (f guess)])
      (if (close-enough? guess next tolerance)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2.0)))

(define (compose f g)
  (lambda (x) (f (g x))))

(define (repeated f n)
  (define id (lambda (x) x))
  (define (odd? x) (= (remainder x 2) 1))
  (define (iter res n_)
    (cond ((= n_ 0) res)
          ((odd? n_) (iter (compose f res)
                           (- n_ 1)))
          (else (iter (compose res res)
                      (/ n_ 2)))))
  (if (= n 0)
      id
      (iter f (- n 1))))

(define (exp b-fe n-fe)
  (define (iter a b n); invariant a*b^n
    (cond ((= n 0) a)
          ((= (remainder n 2) 1)
           (iter (* a b) b (- n 1)))
          (else
           (iter a (* b b) (/ n 2)))))
  (iter 1 b-fe n-fe))

(define (n-root x n first-guess)
  (let ([m (floor (- (log n) (log 2)))])
    (define simple-transform
      (lambda (y) (/ x (exp y (- n 1)))))
    (define (m-da m_)
      (repeated average-damp m_))
    (fixed-point ((m-da m) simple-transform) first-guess)))


(check-true (close-enough?
             2
             (n-root (exp 2 7) 7 1)
             tolerance))
(check-true (close-enough?
             2
             (n-root (exp 2 8) 8 1)
             tolerance))
(check-true (close-enough?
             2
             (n-root (exp 2 9) 9 1)
             tolerance))
