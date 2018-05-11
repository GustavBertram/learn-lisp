#lang sicp
;;;; Exercises and answers from SICP Ch 1.2

;;; Exercise 1.11.
;; A function f is defined by the rule that:
;; f(n) = n if n<3
;; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3.

;; Write a procedure that computes f by means of a recursive process.

(define f (n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* (f (- n 2)) 2)
         (* (f (- n 3)) 3))))

;; Write a procedure that computes f by means of an iterative process. 

