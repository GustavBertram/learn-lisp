;;;; The Magic of Lisp Macros

(defmacro let1 (var val &body body)
  `(let ((,var ,val))
     ,@body))

;; (let ((foo (+ 2 3))) (* foo foo))
;; (let1 foo (+ 2 3) (* foo foo))

;; (macroexpand '(let1 foo (+ 2 3) (* foo foo)))

;;; More complex macros

(defun my-length (lst)
  (labels ((f (lst acc)
             (if lst
                 (f (cdr lst) (1+ acc))
                 acc)))
    (f lst 0)))

;;; Bad split

(defmacro split (val yes no)
  `(if ,val
       (let ((head (car ,val))
             (tail (cdr ,val)))
         ,yes)
       ,no))

(defun my-length1 (lst)
  (labels ((f (lst acc)
             (split lst
                    (f tail (1+ acc))
                    acc)))
    (f lst 0)))

;; This is the problem with split

(split (progn (princ "Lisp rocks!")
              '(2 3))
       (format t "This can be split into ~a and ~a." head tail)
       (format t "This cannot be split."))

;; Better split, evaluates val only once, but captures x
(defmacro split1 (val yes no)
  `(let1 x ,val
     (if x
         (let ((head (car x))
               (tail (cdr x)))
           ,yes)
         ,no)))

;This function is finally safe to use
(defmacro split2 (val yes no)
  (let1 g (gensym)
    `(let1 ,g ,val
       (if ,g
           (let ((head (car ,g))
                 (tail (cdr ,g)))
             ,yes)
           ,no))))

(defun my-length2 (lst)
  (labels ((f (lst acc)
             (split2 lst
                     (f tail (1+ acc))
                     acc)))
    (f lst 0)))

;; Function to help with recurse
(defun pairs (lst)
  (labels ((f (lst acc)
             (split lst
                    (if tail
                        (f (cdr tail) (cons (cons head (car tail)) acc))
                        (reverse acc))
                    (reverse acc))))
    (f lst nil)))

;; Recurse macro
(defmacro recurse (vars &body body)
  (let1 p (pairs vars)
    `(labels ((self ,(mapcar #'car p)
                ,@body))
       (self ,@(mapcar #'cdr p)))))

(recurse (n 9)
  (fresh-line)
  (if (zerop n)
      (princ "lift-off!")
      (progn (princ n)
             (self (1- n)))))

(defun my-length3 (lst)
  (recurse (lst lst
                acc 0)
    (split2 lst
           (self tail (1+ acc))
           acc)))

(my-length3 '(1 2 3 4 5))

;;; There may be simpler ways to do what you need without macros

(defun my-length4 (lst)
  (reduce (lambda (x i)
            (1+ x))
          lst
          :initial-value 0))

(my-length4 '(1 2 3 4 5))
