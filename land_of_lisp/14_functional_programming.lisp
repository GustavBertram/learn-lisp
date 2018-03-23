;;;; Functional programming

;;; Functional style rules

;; 1. Functions always return the same result, as long as the same arguments are passed in.
;; 2. Functions never refrences variables that are defined outside the main function, unless we are certain that these variables will remain constant.
;; 3. No variables are modified or mutated by the function.
;; 4. The purpose of the function is to do nothing except return a result.
;; 5. The function doesn't do anything that is visible to the outside world.
;; 6. The function doesn't take information from an outside source.
;; 7. Break code into functional and imperative parts.

;;; Widget tracker

;; Functional part
(defun add-widget (database widget)
  (cons widget database))

;; Imperative part
(defparameter *db* nil)

(defun main-loop ()
  (loop (princ "Please enter the name of a new widget:")
        (setf *db* (add-widget *db* (read)))
        (format t "The database contains the following: ~a~%" *db*)))

;;; Higher order programming

(defparameter *my-list* '(4 7 2 3))

;; My attempt 1
(map 'list #'(lambda (n) (+ 2 n)) *my-list*)

;; My attempt 2
(defun add-two (lst &optional (acc nil))
  (if lst
      (add-two (cdr lst) (cons (+ 2 (car lst)) acc))
      (reverse acc)))

(add-two *my-list*)

;; From book
(defun add-two-2 (lst)
  (when lst
    (cons (+ 2 (car lst)) (add-two-2 (cdr lst)))))

(add-two-2 *my-list*)

;; From the book
(mapcar #'(lambda (n)
            (+ 2 n))
        *my-list*)

;; Functional optimization techniques: Memoization, tail call optimization, lazy evaluation, higher-order programming.

;; Functional benefits: Reduces bugs, programs more compact, more elegant.
    
