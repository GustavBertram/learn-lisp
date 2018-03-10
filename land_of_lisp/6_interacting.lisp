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

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (sexp)
  (if (member (car sexp) *allowed-commands*)
      (eval sexp)
      '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond ((eq item #\space)            (cons item (tweak-text rest caps lit)))
            ((member item '(#\! #\? #\.)) (cons item (tweak-text rest t lit)))
            ((eq item #\")                (tweak-text rest caps (not lit))) ; Don't cons
            (lit                          (cons item (tweak-text rest nil lit))) ; No change
            ;; Next cond was (or caps lit), but seems wrong
            (caps                         (cons (char-upcase item) (tweak-text rest nil lit)))
            (t                            (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (lst)
  (princ (coerce (tweak-text (coerce (string-trim "() "
                                                  (prin1-to-string lst))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

;; (coerce "test123" 'list) ; => (#\t #\e #\s #\t #\1 #\2 #\3)
;; Since it's coerced to a list of characters, can work on them one by one.

;;(coerce (list (char-upcase #\t) (char-downcase #\E) #\s #\t #\1 #\2 #\3) 'string) ; => "Test123"

;(let ((lit 1))
;  (cond (lit 'a)
;        ((or nil lit) 'b)))

;; Found earlier, to be used later
(defun game-repl ()
  (let ((cmd (game-read)))
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))
