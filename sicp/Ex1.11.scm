#lang sicp
;;;; Exercises and answers from SICP Ch 1.2

;;; Exercise 1.11.
;; A function f is defined by the rule that:
;; f(n) = n if n<3
;; f(n) = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n> 3.

;; Write a procedure that computes f by means of a recursive process.

(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1))
         (* (f (- n 2)) 2)
         (* (f (- n 3)) 3))))

;; Write a procedure that computes f by means of an iterative process. 

(define (f-iter n)
  (define (fi i a b c)
    (if (< i n)
        (fi (inc i) b c (+ c
                           (* b 2)
                           (* a 3)))
        c))
  
  (if (< n 3)
      n
      (fi 2 0 1 2)))
  
  