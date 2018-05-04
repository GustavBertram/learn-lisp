;;;; CLOSURES

;;; Lexical and dynamic scope

(let ((x 2))
  x)

(defvar temp-special 1)

(defun temp-special-returner ()
  temp-special)

(temp-special-returner)

(let ((temp-special 2))
  (temp-special-returner))

;;; Lambda

;; (function '(lambda (x) (+ 1 x))) ; Doesn't actually work
(function (lambda (x) (+ 1 x)))

;; Equivalent to:
#'(lambda (x) (+ 1 x))

;; Equivalent to:
(lambda (x) (+ 1 x))

;; Can't call a function to dynamically retirn the symbol to be used in a regular function call (why not?)
;; Use FUNCALL or APPLY for this.


