#lang sicp
;;;; Exercises and answers from SICP Ch 1.1

;;; Exercise 1.8.
;; Newton's method for cube roots is based on the fact that if y is an
;; approximation to the cube root of x, then a better approximation is given by
;; the value:
;; (x/y^2 + 2y)/3

;; Use this formula to implement a cube-root procedure analogous to the square-root procedure.

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (cube-root x)

  (define (good-enough? prev guess)
    (< (abs (- prev guess)) 0.0001))

  (define (improve-guess guess)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))
  
  (define (cube-root-iter prev guess)
    (if (good-enough? prev guess)
        guess
        (cube-root-iter guess (improve-guess guess))))

  (cube-root-iter 0 1.0))
