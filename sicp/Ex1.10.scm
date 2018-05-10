#lang sicp
;;;; Exercises and answers from SICP Ch 1.2

;;; Exercise 1.10.
;; The following procedure computes a mathematical function called Ackermann's
;; function:

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

; Looks like it's the Tarjan variant of the Ackermann function. (https://www.encyclopediaofmath.org/index.php/Ackermann_function)

; R.E. Tarjan, "Efficiency of a good but not linear set union algorithm" J. Assoc. Comput. Mach. , 22 (1975) pp. 215–225
; Paper included in this repo as "Ex1.10_p215-tarjan.pdf" by permission of the ACM.

;; What are the values of the following expressions?

(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
; 2^10 = 1024
; The process is linear recursive.
; T: O(n)
; S: O(n)

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
; 2^16 = 65536
; The process is tree recursive.
; T: O(n^2) because it needs to calculate n^2 final steps.
; S: O(n^2) because it needs to go n^2 deep when calculating the final steps.
   
(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 3 1)))
(A 2 (A 2 2))
;...
(A 2 4)
;...
; 2^16 = 65536S
   
;; Consider the following procedures, where A is the procedure defined above.
;; Give concise mathematical definitions for the functions computed by the
;; procedures f, g, and h for positive integer values of n.
;; For example, (k n) computes 5n^2. 

(define (f n) (A 0 n)) ; 2n

(define (g n) (A 1 n)) ; 2^n where n>0

(define (h n) (A 2 n)) ; 2^^n (tetration), where n>0 and ^^ is Knuth up arrows

; (A 3 n) computes 2^^^n (pentation)

; I think this version of Ackermann's function calculates 2\uparrow^n (https://en.wikipedia.org/wiki/Knuth%27s_up-arrow_notation)
; This seems to confirm that idea: https://github.com/sarabander/p2pu-sicp/blob/master/1.2/Ex1.10.pdf 

(define (k n) (* 5 n n)) ; 5n^2




