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
