#lang sicp

(#%require rackunit)

;Exercise 1.44

;The idea of smoothing a function is an important concept in signal processing.
;If f is a function and dx is some small number, then the smoothed version of f
;is the function whose value at a point x is the average of f (x − dx), f (x),
;and f (x+dx). Write a procedure smooth that takes as input a procedure that
;computes f and returns a procedure that computes the smoothed f . It is
;sometimes valuable to repeatedly smooth a function (that is, smooth the
;smoothed function, and so on) to obtain the n-fold smoothed function. Show how
;to generate the n-fold smoothed function of any given function using smooth
;and repeated from Exercise 1.43.

;Solution

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

(define (smooth f)
  (let ([dx 1.0])
    (lambda (x) (/ (+ (f (- x dx))
                      (f x)
                      (f (+ x dx)))
                   3.0))))

(define (n-smooth n)
  (repeated smooth n))

(define (close-enough? v1 v2 t)
  (< (abs (- v1 v2))
     t))

(define tolerance 0.0001)

(define lin-2x (lambda (x) (* x 2)))
(define square (lambda (x) (* x x)))

(check-true (close-enough?
             ((smooth lin-2x) 2)
             4
             tolerance))
(check-true (close-enough?
             ((smooth lin-2x) 5)
             10
             tolerance))
(check-true (close-enough?
             ((smooth square) 0)
             (/ 2 3)
             tolerance))

(check-equal? (((n-smooth 3) square) 15)
              ((smooth (smooth (smooth square))) 15))