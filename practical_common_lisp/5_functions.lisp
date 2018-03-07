(defun verbose-sum (x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y))

;; Optional parameters

(defun opt-foo (a b &optional (c 3 c-supplied-p))
  (list a b c c-supplied-p))

;; Rest parameters

(defun rest-foo (&rest rest)
  rest) ; rest is already a list

(rest-foo 1 2 3 4 'b)

;; Keyword parameters

(defun key-foo (&key (a 0) (b 0 b-supplied-p) (c (+ a b)))
  (list a b c b-supplied-p))

(key-foo :a 1 :b 2) ; => 1 2 3 T

;; Possible combinations of parameter types:
;; Required, &optional, &rest, &key.
;; Required and &optional / &rest / &key typical.
;; &optional and &rest.

;; Optional / rest and key combined have surprising results.

;; Generally use &key parameters for optionals - you can add another &key without disturbing existing calls.


;; Returning

(defun ret-foo (n)
  (dotimes (i 10)
    (dotimes (j 10)
      (when (> (* i j) n)
        (return-from ret-foo (list i j))))))

(ret-foo 4)

;;;; Functions as data

(defun sqr-foo (x) (expt x 2))

;; function
(function sqr-foo)
#'foo ; Equivalent to above

;; funcall
(funcall #'sqr-foo 4) ; === (sqr-foo 4)

(defun plot (fn min max step)
  (loop for i from min to max by step do
    (loop repeat (funcall fn i) do (format t "*"))
    (format t "~%")))

(plot #'exp 0 4 1/2)

;; apply
(defparameter plot-data '(exp 0 4 1/2))

(apply #'plot '(exp 0 4 1/2))
(apply #'plot #'exp '(0 4 1/2))

;; lambda functions

(funcall #'(lambda (x y) (+ x y)) 2 3)
((lambda (x y) (+ x y)) 2 3)

(plot #'(lambda (x) (* 2 x)) 0 10 1)

