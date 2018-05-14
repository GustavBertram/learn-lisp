#lang sicp
;;;; Exercises and answers from SICP Ch 1.2

;;; Exercise 1.12.
;; The following pattern of numbers is called Pascal's triangle:

;;     1
;;    1 1
;;   1 2 1
;;  1 3 3 1
;; 1 4 6 4 1

;; The numbers at the edge of the triangle are all 1, and each number inside the triangle is the sum of the two numbers above it.

;; Write a procedure that computes elements of Pascal's triangle by means of a recursive process.

; Assume an (n,m) mapping for n'th row and m'th number in row, starting at 0,0

(define (pascal n m)
  (cond ((< n 0) 0) ; Out of bounds
        ((< n m) 0) ; Out of bounds
        ((= m 0) 1)
        ((= m n) 1)
        (else (+ (pascal (dec n) (dec m))
                 (pascal (dec n) m)))))

