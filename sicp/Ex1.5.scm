#lang sicp
;;;; Exercises and answers from SICP Ch 1.1

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


