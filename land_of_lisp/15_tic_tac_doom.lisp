;;;; TIC TAC DOOM
;;;; Reimplementation of Dice of Doom for Tic Tac Toe

(defparameter *players* 2)
(defparameter *board-size* 2)
(defparameter *board-squares* (* *board-size* *board-size*))

;;; Representing the board

;; 0|1
;; -+-
;; 2|3

;; board : (nil nil nil 0) ; (player)

;; 0|1|2
;; -+-+-
;; 3|4|5
;; -+-+-
;; 6|7|8

(defun player-letter (n)
  (case n
    (0 #\O)
    (1 #\X)
    (otherwise #\SPACE)))

(defun get-square (board x y)
  (nth (+ x (* y *board-size*)) board))

(defun draw-divider ()
  (loop repeat *board-size*
        do (princ "+-"))
  (princ "+"))

(defun draw-board (board)
  (draw-divider)
  (loop for y below *board-size*
        do (progn (fresh-line)
                  (princ "|")
                  (loop for x below *board-size*
                        for square = (get-square board x y)
                        do (format t "~a|" (player-letter square)))
                  (fresh-line)
                  (draw-divider)))
  (fresh-line))

; (draw-board '(nil nil nil 0))

;;; Generate the game tree

(defun board-move (board player move)
  (loop for n below *board-squares*
        collect (if (= n move)
                    player
                    (nth n board))))

(defun add-moves (board player)
  (mapcan (lambda (pos)
            ;(format t "BOARD:~a POS:~a ~%" board pos)
            (when (not (nth pos board))
              (list
               (list
                pos
                (game-tree (board-move board player pos)
                           (mod (1+ player) *players*))))))
          (loop for n below *board-squares*
                collect n)))

;(add-moves '(0 0 nil nil) 1) ;=>
;((2 ;move
;    (0 ;player
;     (0 0 1 NIL) ;board
;     ((3 (1 (0 0 1 0) NIL)))))
; (3 (0 ;move
;     (0 0 NIL 1) ;player
;     ((2 (1 (0 0 0 1) NIL)))))) ;board

(defun game-tree (board player)
  ;(format t "BOARD:~a PLAYER:~a ~%" board player)
  ;(draw-board board)
  (list player
        board
        (add-moves board
                   player)))

;;; Main loop

(defun handle-human (tree)
  (fresh-line)
  (princ "Choose your move:")
  (let ((moves (caddr tree)))
    (loop for move in moves
          for n from 1
          do (let ((action (car move)))
               (fresh-line)
               (format t "~a. " n)
               (if action
                   (format t "~a" action)
                   (princ "End turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))


(defun print-info (tree)
  (fresh-line)
  (format t "Current player = ~a~%" (player-letter (car tree)))
  (draw-board (cadr tree)))

(defun play-vs-human (tree)
  (print-info tree)
  (if (caddr tree)
      (play-vs-human (handle-human tree))
      ;(announce-winner (cadr tree))
      'DONE
      ))

; (play-vs-human (game-tree '(nil nil nil nil) 0))
