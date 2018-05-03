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

;;; Dynamic / special variables

;; Will be set once
(defvar *count* 0
  "Count of widgets made so far.")

;; Will be set every time it's evaluated
(defparameter *gap-tolerance* 0.001
  "Tolerance allowed in widget gaps.")

(defvar *x* 10)
(defun foo ()
  (format t "X: ~d~%" *x*))

(foo)

(let ((*x* 20))
  (foo))

(defun bar ()
  (foo)
  (let ((*x* 20)) (foo))
  (foo))

(bar)

(defun foo ()
  (format t "Before assignment~18tX: ~d~%" *x*)
  (setf *x* (+ 1 *x*))
  (format t "After assignment~18tX: ~d~%" *x*))

(bar)

(defconstant +pi+ 22/7
  "Aproximate value of Pi. Very aproximate.")

;; (let ((+pi+ 3))) ; Throws an error

;;; Assignment

;; (setf <place> <value>)

;; Can set multiple values at once
(setf x 1 y 2)

(incf x)
(decf y)

;; Swap values
(rotatef x y)

;; Shift values over
(shiftf x y 20)
