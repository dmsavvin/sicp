#lang sicp

(#%require rackunit)

;Exercise 1.29

;Simpson’s Rule is a more accurate method of numerical integration than the
;method illustrated above. Using Simpson’s Rule, the integral of a function f
;between a and b is approximated as
;(h/3)*(y_0 + 4*y_1 + 2*y_2 + 4*y_3 + 2*y_4 + ... + 2*y_{n-2} + 4*y_{n-1} + y_n),
;where h = (b − a)/n, for some even integer n, and y_k = f(a + kh). (Increasing
;n increases the accuracy of the approximation.) Define a procedure that takes
;as arguments f , a, b, and n and returns the value of the integral, computed
;using Simpson’s Rule. Use your procedure to integrate cube between 0 and 1
;(with n = 100 and n = 1000), and compare the results to those of the integral
;procedure shown above.

;Solution

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (integral f a b dx)
  (define (add-dx x)
    (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(define (qube x) (* x x x))

;Simpson’s Rule numerical integration
(define (sr-int f a b n)
  (define dx (/ (- b a) n))
  (define halve-dx (/ (/ (- b a) n) 2))
  (define (sr-simple g start)
    (* (/ dx 3)
       (+ (g start)
          (* (g (+ start dx)) 4)
          (g (+ start dx dx)))))
  (define (rec g start finish)
    (if (> (+ halve-dx start) finish)
        0.0
        (+ (sr-simple g start)
           (rec g (+ start (* dx 2)) finish))))
  (rec f a b))

;Returns the procedure that check whether the first argument is closer to target
;than the second argument
(define (is-closer-to target)
  (lambda (a b) (< (abs (- target a)) (abs (- target b)))))

(define apr-100 (integral qube 0 1 0.01))
(define apr-1000 (integral qube 0 1 0.001))
(define sr-apr-100 (sr-int qube 0 1 100))
(define sr-apr-1000 (sr-int qube 0 1 1000))

(check (is-closer-to 0.25) sr-apr-100 apr-100)
(check (is-closer-to 0.25) sr-apr-1000 apr-1000)