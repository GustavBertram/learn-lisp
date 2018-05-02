;;;; VARIABLES

;;; LET and LET*

(let ((x 10)
      (y 20)
      z)
  (setf z (+ x y))
  z)

(defun foo (x)
  (format t "Parameter: ~a~%" x) ;x is argument
  (let ((x 2))
    (format t "Outer LET: ~a~%" x) ;x is 2
    (let ((x 3))
      (format t "Inner LET: ~a~%" x)) ;x is 3
    (format t "Outer LET: ~a~%" x))
  (format t "Parameter: ~a~%" x))

(foo 1)

;;; LET vs LET*

(let* ((x 10)
       (y (+ x 10)))
  (list x y))

(let ((x 10))
  (let ((y (+ x 10)))
    (list x y)))

;;; Lexical variables and closures

(defparameter *fn*
  (let ((count 0))
    #'(lambda () (setf count (1+ count)))))

(funcall *fn*)

;; A multiple closures can capture the same binding
(let ((count 0))
  (list
   #'(lambda () (incf count))
   #'(lambda () (decf count))
   #'(lambda () count)))

;;; Dynamic or special variables

