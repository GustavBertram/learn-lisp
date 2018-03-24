;;;; Dice of Doom 1

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;;; Representing the board

;;     ___ 
;; ___/ 0 \
;;/ 2 \___/
;;\___/ 1 \
;;/ 3 \___/
;;\__/

;; board: ((0 3) (0 3) (1 3) (1 1)) ; (player . dice)

(defun board-array (lst)
  "(Functional) Converts board list into a faster-access array"
  (make-array *board-hexnum* :initial-contents lst))

(defun gen-board ()
  "(Imperative) Randomizes board for starting position"
  (board-array (loop for n below *board-hexnum*
                     collect (list (random *num-players*)
                                   (1+ (random *max-dice*))))))

(defun player-letter (n)
  "(Functional) Convert player number into letter"
  (code-char (+ 97 n)))

(defun draw-board (board)
  "(Imperative)"
  (loop for y below *board-size*
        do (progn (fresh-line)
                  (loop repeat (- *board-size* y)
                        do (princ " "))
                  (loop for x below *board-size*
                        for hex = (aref board (+ x (* *board-size* y)))
                        do (format t "~a-~a " (player-letter (first hex))
                                   (second hex))))))
