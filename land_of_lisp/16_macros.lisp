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


