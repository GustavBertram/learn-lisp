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


