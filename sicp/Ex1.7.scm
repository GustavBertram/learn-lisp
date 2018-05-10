#lang sicp
;;;; Exercises and answers from SICP

;;; Exercise 1.7.

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  
  (define (improve guess)
    (average guess (/ x guess)))
  
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  
  (sqrt-iter 1.0))

;; The good-enough? test used in computing square roots will not be very
;; effective for finding the square roots of very small numbers. Also, in real
;; computers, arithmetic operations are almost always performed with limited
;; precision. This makes our test inadequate for very large numbers.

;; Explain these statements, with examples showing how the test fails for small
;; and large numbers.

; For very small numbers, the precision of the current good-enough? is too low.
; It will stop too early in the iteration, and give answers that are not good
; enough.
; (square (sqrt 0.0001))
; 0.0010438358335233748 ; Response is more than an order of magnitude out.

; For very large numbers, since the guess is a float, it might lose necessary
; bits to get it closer to the right answer, and so never stop iterating.
; (sqrt 10000000000000) ; Goes into an infinite loop

;; An alternative strategy for implementing good-enough? is to watch how guess
;; changes from one iteration to the next and to stop when the change is a very
;; small fraction of the guess.

;; Design a square-root procedure that uses this kind of end test.

(define (sqrt-diff x)

  (define (good-enough? prev guess)
    (< (abs (- prev guess)) 0.001))
  
  (define (improve guess)
    (average guess (/ x guess)))
  
  (define (sqrt-iter prev guess)
    (if (good-enough? prev guess)
        guess
        (sqrt-iter guess (improve guess))))
  
  (sqrt-iter 0 1.0))


;; Does this work better for small and large numbers? 

; It doesn't go into an infinite loop for larger numbers, which I expected.

; Surprisingly, it works better for smaller numbers too:
; (square (sqrt-diff 0.0001)) ; 0.00010001428128408621
