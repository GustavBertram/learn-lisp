#lang sicp
;;;; Exercises and answers from SICP Ch 1.1

;;; Exercise 1.3.
;; Define a procedure that takes three numbers as arguments and returns the sum
;; of the squares of the two larger numbers. 

(define (sum-sqr a b)
  (+ (* a a)
     (* b b)))

(define (sum-sqr-of-two-largest a b c)
  (cond ((and (<= a b) (<= a c)) (sum-sqr b c))
        ((and (<= b a) (<= b c)) (sum-sqr a c))
        ((and (<= c a) (<= c b)) (sum-sqr a b))))