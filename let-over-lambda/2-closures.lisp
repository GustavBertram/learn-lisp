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

;;; Lambda folding -------------------------------------------------------------

(defun compiler-test ()
  (funcall
    (lambda (x) (+ 1 x))
    2))

;; (disassemble 'compiler-test)

(defun lambda-returner ()
  (lambda (x) (1+ x))) ; No overhead

(funcall (lambda-returner) 2)

(defun let-over-lambda-returner (z)
  (let ((y z)) ; Creates a new environment
    (lambda (x)
      (incf y x))))

(funcall (let-over-lambda-returner 3) 2)

(progn
  (compile 'let-over-lambda-returner)
  (time (let-over-lambda-returner 4))) ; 0 bytes consed?

;;; Let over lambda ------------------------------------------------------------

(let ((x 0))
  (lambda () x))

(let ((counter 0))
  (values
   (lambda () (incf counter))
   (lambda () (decf counter))))

;;; Lambda over let over lambda

;; Lambda expressions are constants, pointers to machine code
(lambda ()
  (let ((counter 0))
    (lambda () (incf counter))))

(defun make-counter () ; Data attached to a function without an object
  (let ((counter 0))
    (lambda () (incf counter)))) 

;; Non-trivial example of function with attached data
(defun block-scanner (trigger-string)
  (let* ((trig (coerce trigger-string 'list))
         (curr trig))
    (lambda (data-string)
      (let ((data (coerce data-string 'list)))
        (dolist (c data)
          (if curr
              (setq curr
                    (if (char= (car curr) c)
                        (cdr curr)      ; next char
                        trig))))        ; start over 
        (not curr)))))                  ; return t if found

(defvar scanner
  (block-scanner "test"))

(funcall scanner "We will start ") ; NIL
(funcall scanner "the te")         ; NIL
(funcall scanner "st tomorrow.")   ; T

;;; Let over lambda over let over lambda

;; A way to implement class variables, or static variables
(let ((direction 'up))
  (defun toggle-counter-direction ()
    (setq direction
          (if (eq direction 'up)
              'down
              'up)))

  (defun counter-class ()
    (let ((counter 0))
      (lambda ()
        (if (eq direction 'up)
            (incf counter)
            (decf counter))))))






