;; READ PRINT PRIN1 PRINC

;; When you need to print something, first consider the PRINT command
;; When you need to read something, first consider the READ command
;; They can feed into one another.

;; PRINT outputs machine-readable formats
;; READ inputs machine readable formats

(print '((a) b c ((d e) f)))

(defun say-hello ()
  (print "Please type your name:")
  (let ((name (read)))
    (print "Nice to meet you, ")
    (print name)))

(defun add-five ()
  (print "please enter a number:")
  (let ((num (read)))
    (print "When I add five I get")
    (print (+ num 5))))

;; EVAL

(defparameter *foo* '(+ 1 2))
(eval *foo*)

;; Game REPL

(defun basic-game-repl ()
  (loop (print (eval (read))))) ;; READ, EVAL, PRINT, LOOP

;; READ-FROM-STRING

(defun game-read ()
  (let ((cmd (read-from-string ;; reads Lisp from a string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

;; Found earlier, to be used later
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))
