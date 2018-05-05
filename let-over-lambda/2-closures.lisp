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

((lambda (x) (+ 1 x)) 2)

;;; Lambda folding

(defun compiler-test ()
  (funcall
    (lambda (x) (+ 1 x))
    2))

;; (disassemble 'compiler-test)

(defun lambda-returner ()
  (lambda (x) (1+ x))) ; No overhead

(funcall (lambda-returner) 2)

(defun let-over-lambda-returner ()
  (let ((y 1)) ; Creates a new environment
    (lambda (x)
      (incf y x))))

(funcall (let-over-lambda-returner) 2)

(progn
  (compile 'let-over-lambda-returner)
  (time (let-over-lambda-returner))) ; 0 bytes consed

(compile 'let-over-lambda-returner) ; OK, this is not supposed to work


