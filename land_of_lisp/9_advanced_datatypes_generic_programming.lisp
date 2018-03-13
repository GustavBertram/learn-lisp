;;;; ADVANCED DATATYPES AND GENERIC PROGRAMMING

;;; Arrays

(defparameter x (make-array 3))
(aref x 1)
(setf (aref x 1) 10)

;; Generic setters

(setf foo '(a b c))
(setf (second foo) 'z)
(setf (third foo) (make-array 4))
foo ;=> (A Z #(0 0 0 0))


;;; Hash tables

(defparameter y (make-hash-table))
(gethash 'yup y)
(setf (gethash 'nope y) 'refused)

;; Returning multiple values

(defun foof ()
  (values 3 7))

(multiple-value-bind (a b) (foof)
  (* a b))

;; Faster grand theft wumpus

(defun hash-edges (edge-list)
  (let ((tab (make-hash-table)))
    (mapc (lambda (x)
            (let ((node (car x)))
              (push (cdr x) (gethash node tab))))
          edge-list)
    tab))

(defun get-connected-hash (node edge-tab)
  (let ((visited (make-hash-table)))
    (labels ((traverse (node)
               (unless (gethash node visited)
                 (setf (gethash node visited) t)
                 (mapc (lambda (edge)
                         (traverse edge))
                       (gethash node edge-tab)))))
      (traverse node))
    visited))

;(time (dotimes (i 100)
;        (get-connected-hash 1 (hash-edges (make-edge-list)))))

;;; Common Lisp Structures

(defstruct person
  name ; slots
  age
  waist-size
  favorite-color) 

(defparameter *bob*
  (make-person :name "Bob"
               :age 35
               :waist-size 32
               :favorite-color "blue"))

*bob*
(person-age *bob*)
(defparameter *that-guy* #S(person :name "Bob" :age 35 :waist-size 32 :favorite-color "blue"))
(person-age *that-guy*)

;;; Handling Data in a Generic Way

;; LENGTH works with sequences (lists, arrays and sequences)

(length '(a b c))
(length "blub")
(length (make-array 5))

;; Sequence functions for searching
(find-if #'numberp '(a b 5 d)) ; first value that satisfies predictate
(count #\s "mississippi") ; how often a certain object appears in sequence
(position #\4 "2kewl4skewl") ; position in sequence
(some #'numberp '(a b 5 d)) ; whether some values satisfy a predictate
(every #'numberp '(a b 5 d)) ; whether all values satisfy a predictate

;; Sequence Functions for Iterating Across a Sequence

(reduce #'+ '(3 4 6 5 2))

(defun sum (lst)
  (reduce #'+ lst))

(map 'list ; mapcar that works across all sequences, not just lists
     (lambda (x)
       (if (eq x #\s)
           #\S
           x))
     "this is a string")

;; Two More Important Sequence Functions
(subseq "america" 2 6)
(sort '(5 8 2 4 9 3 6) #'<)

;; Type Predicates

; ARRAYP CHARACTERP CONSP FUNCTIONP HASH-TABLE-P LISTP SYMBOLP

(defun add (a b)
  (cond ((and (numberp a) (numberp b)) (+ a b))
        ((and (listp a) (listp b)) (append a b))))

; DEFMETHOD defines multiple versions of a function for different types
(defmethod add ((a number) (b number))
  (+ a b))

(defmethod add ((a list) (b list))
  (append a b))

(add 3 4)
(add '(a b) '(c d))



