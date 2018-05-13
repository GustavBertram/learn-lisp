;;;; LAZY EVALUATION
;;; Contains code from Land of Lisp Ch 18

(defmacro lazy (&body body)
  (let ((forced (gensym))
        (value (gensym)))
    `(let ((,forced nil)
           (,value nil))
       (lambda ()
         (unless ,forced
           (setf ,value (progn ,@body))
           (setf ,forced t))
         ,value))))

(defun force (lazy-value)
  (funcall lazy-value))

;; (lazy (+ 1 2))
;; (force (lazy (+ 1 2)))

;;(defun add (a b)
;;  (princ "I am adding now")
;;  (+ a b))

;; (defparameter *foo* (lazy (add 1 2)))
;; (force *foo*)

;;; Lazy Lists -----------------------------------------------------------------

(defmacro lazy-cons (a d)
  `(lazy (cons ,a ,d)))

(defun lazy-car (x)
  (car (force x)))

(defun lazy-cdr (x)
  (cdr (force x)))

;; (defparameter *foo* (lazy-cons 4 7))
;; (lazy-car *foo*)
;; (lazy-cdr *foo*)

(defparameter *integers*
  (labels ((f (n)
             (lazy-cons n (f (1+ n)))))
    (f 1)))

;; (lazy-car (lazy-cdr (lazy-cdr *integers*)))

(defun lazy-nil ()
  (lazy nil))

(defun lazy-null (x)
  (not (force x)))

;;; Converting between lists and lazy lists ------------------------------------

(defun make-lazy (lst)
  (lazy (when lst
          (cons (car lst) (make-lazy (cdr lst))))))

(defun take (n lst)
  (unless (or (zerop n) (lazy-null lst))
    (cons (lazy-car lst) (take (1- n) (lazy-cdr lst)))))

(defun take-all (lst)
  (unless (lazy-null lst)
    (cons (lazy-car lst) (take-all (lazy-cdr lst)))))

;; (take 10 *integers*)
;; (take 10 (make-lazy '(q w e r t y u i o p a s d f)))
;; (take-all (make-lazy '(q w e r t y u i o p a s d f)))

;;; Mapping and searching across lazy lists ------------------------------------

(defun lazy-mapcar (fun lst)
  (lazy (unless (lazy-null lst)
          (cons (funcall fun (lazy-car lst))
                (lazy-mapcar fun (lazy-cdr lst))))))

(defun lazy-mapcan (fun lst)
  (labels ((f (lst-cur)
             (if (lazy-null lst-cur)
                 (force (lazy-mapcan fun (lazy-cdr lst)))
                 (cons (lazy-car lst-cur) (lazy (f (lazy-cdr lst-cur)))))))
    (lazy (unless (lazy-null lst)
            (f (funcall fun (lazy-car lst)))))))

(defun lazy-find-if (fun lst)
  (unless (lazy-null lst)
    (let ((x (lazy-car lst)))
      (if (funcall fun x)
          x
          (lazy-find-if fun (lazy-cdr lst))))))

(defun lazy-nth (n lst)
  (if (zerop n)
      (lazy-car lst)
      (lazy-nth (1- n) (lazy-cdr lst))))

;; (take 10 (lazy-mapcar #'sqrt *integers*))
; (1.0 1.4142135 1.7320508 2.0 2.236068 2.4494898 2.6457512 2.828427 3.0 3.1622777)

