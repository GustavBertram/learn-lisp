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

; Need to know the outer edges to return 1 for them.

;     0
;    1 2
;   3 4 5
;  6 7 8 9

; Left edge (0,1,3,6,...) are triangle numbers: https://oeis.org/A000217

; Formula for triangle numbers: x = n(n+1)/2
; Solved for n: n = (sqrt(8*x+1)-1)/2

; When n is an integer, x is a triangle number.
; If x+1 is a triangle number, it's on the other edge (0,2,5,9,...)

(define (triangle-number? n)
  (integer? (/ (- (sqrt (+ (* 8 n) 1) ) 1) 2)))

; Need to get the row, to get to the parent numbers

;   n: 0 1 2 3 4 5 6 7 8 9
; row: 0 1 1 2 2 2 3 3 3 3

; Matches this sequence: https://oeis.org/A003056
; Formula: r = floor((sqrt(1+8*n)-1)/2)

(define (pascal-row n)
  (floor (/ (- (sqrt (+ 1 (* 8 n))) 1) 2)))

; Now we can calulate pascal's triangle with a single parameter of n.

(define (pascaln n)
  (cond ((< n 0) 0) ; Out of bounds
        ((= n 0) 1) ; Base case
        ((or (triangle-number? n) (triangle-number? (inc n))) 1)
        (else (+ (pascaln (- n (pascal-row n) 1))
                 (pascaln (- n (pascal-row n)))))))
    
  
    

      