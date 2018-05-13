#lang sicp
;;;; Exercises and answers from SICP Ch 1.2

;;; Exercise 1.13.
;; Prove that Fib(n) is the closest integer to p^n/5, where p = (1 + sqrt(5))/2.

;; Hint: Let s = (1 - sqrt(5))/2.
;; Use induction and the definition of the Fibonacci numbers (see section 1.2.2) to prove that Fib(n) = (p^n - s^n)/sqrt(5). 