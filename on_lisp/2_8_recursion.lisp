;; TCO implemented in SBCL: https://0branch.com/notes/tco-cl.html#sec-2-2
;; Rewrite code to use loops: https://stackoverflow.com/questions/23175459/why-does-tco-require-support-from-the-vm
;; Why does TCO require support from the VM? https://stackoverflow.com/questions/23175459/why-does-tco-require-support-from-the-vm
;; How to tell if my code has been TCOd? https://stackoverflow.com/questions/49066791/how-can-i-tell-if-my-function-has-been-tail-call-optimized 

(proclaim '(optimize speed))

(defun our-length (lst)
  "not tail recursive"
  (if (null lst)
      0
      (1+ (our-length (cdr lst))))) ; Return passes to +1

(our-length '(a b c d e f))

(defun our-length-tr (lst)
  "tail recursive version with accumulator"
  (labels ((rec (lst acc)
             (if (null lst) ;if list is empty
                 acc        ;return acc
                 (rec (cdr lst) (1+ acc))))) ;else recurse on rest of list, with acc+1
    (rec lst 0))) ;call rec on list

(our-length-tr '(a b c d e f))

(defun foo () (foo))
(disassemble 'foo) ; Notice the lack of a CALL

(defun bar () (+1 1))
(disassemble 'bar) ; Has a CALL
