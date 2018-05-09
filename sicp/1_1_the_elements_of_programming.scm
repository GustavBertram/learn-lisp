#lang sicp

;;;; 1.1 THE ELEMENTS OF PROGRAMMING
;;;; Contains selected exercises and answers from SICP


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


;;; Exercise 1.4.
;; Observe that our model of evaluation allows for combinations whose operators
;; are compound expressions. Use this observation to describe the behavior of
;; the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; Returns the absolute value of B added to A.


;;; Exercise 1.5
;; Ben Bitdiddle has invented a test to determine whether the interpreter he is
;; faced with is using applicative-order evaluation or normal-order evaluation.
;; He defines the following two procedures:

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

;; Then he evaluates the expression:

;; (test 0 (p))

;; What behavior will Ben observe with an interpreter that uses
;; applicative-order evaluation? What behavior will he observe with an
;; interpreter that uses normal-order evaluation? Explain your answer. (Assume
;; that the evaluation rule for the special form if is the same whether the
;; interpreter is using normal or applicative order: The predicate expression is
;; evaluated first, and the result determines whether to evaluate the consequent
;; or the alternative expression.)

; Applicative-order evaluates operator, then expressions, then copies body.
; Because it will evaluate test, then 0, then (p), it will go into an infinite
; loop. It's a more iterative process.

; 1. Evaluate operator and operands.
; 2. Substitute operand _results_ for parameters in operator body.
; 3. Evaluate resulting expression.

; Normal-order evaluation _won't evaluate the operands_ until their values were
; needed. It's a more recursive process.

; 1. Evaluate operator.
; 2. Substitute operand _expressions_ for parameters in operator body, until
;    obtaining an expression containing only primitive operations.
; 3. Evaluate the resulting expression


;(test 0 (p))
;(if (= 0 0) 0 (p)) ; blind substitution into test
;0


;;; Exercise 1.6.
;; Alyssa P. Hacker doesn't see why if needs to be provided as a special form.
;; "Why can't I just define it as an ordinary procedure in terms of cond?".
;; Alyssa's friend Eva Lu Ator claims this can indeed be done, and she defines
;; a new version of if:

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

;; Eva demonstrates the program for Alyssa:

;(new-if (= 2 3) 0 5)
;5

;(new-if (= 1 1) 0 5)
;0

;; Delighted, Alyssa uses new-if to rewrite the square-root program:

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y)
     2))

(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (new-sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (new-sqrt-iter (improve guess x)
                     x)))

;; What happens when Alyssa attempts to use this to compute square roots? Explain.

; Infinite loop, since the call to NEW-IF evaluates both operands, always.



