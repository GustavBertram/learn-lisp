;;;; Dice of Doom 1

(defparameter *num-players* 2)
(defparameter *max-dice* 3)
(defparameter *board-size* 2)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;;; Representing the board

;; ___
;;/ 2 \___
;;\___/ 0 \
;;/ 3 \___/
;;\___/ 1 \
;;    \___/

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
;;; Generating the game tree

(defun add-new-dice (board player spare-dice)
  (labels ((f (lst n)
             (cond ((null lst) nil)
                   ((zerop n) lst)
                   (t (let ((cur-player (caar lst))
                            (cur-dice (cadar lst)))
                        (if (and (eq cur-player player) (< cur-dice *max-dice*))
                            (cons (list cur-player (1+ cur-dice))
                                  (f (cdr lst) (1- n)))
                            (cons (car lst) (f (cdr lst) n))))))))
    (board-array (f (coerce board 'list) spare-dice))))

(defun board-attack (board player src dst dice)
  (board-array (loop for pos from 0 ; "for pos" works in CLISP, but SBCL wants "from 0" added
                     for hex across board
                     collect (cond ((eq pos src) (list player 1))
                                   ((eq pos dst) (list player (1- dice)))
                                   (t hex)))))

(defun neighbors (pos)
  (let ((up (- pos *board-size*))
        (down (+ pos *board-size*)))
    (loop for p in (append (list up down)
                           (unless (zerop (mod pos *board-size*))
                             (list (1- up) (1- pos)))
                           (unless (zerop (mod (1+ pos) *board-size*))
                             (list (1+ pos) (1+ down))))
          when (and (>= p 0) (< p *board-hexnum*))
            collect p)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (mapcan (lambda (src)
              (when (eq (player src) cur-player)
                (mapcan (lambda (dst)
                          (when (and (not (eq (player dst) cur-player))
                                     (> (dice src) (dice dst)))
                            (list
                             (list
                              (list src dst)
                              (game-tree (board-attack board cur-player src dst (dice src))
                                         cur-player
                                         (+ spare-dice (dice dst))
                                         nil)))))
                        (neighbors src))))
            (loop for n below *board-hexnum*
                  collect n))))

;(attacking-moves #((0 1) (1 1) (0 2) (1 1)) 0 0)

;(((2 3) ;m?
;  (0    ;p
;   #((0 1) (1 1) (0 1) (0 1)) ;b
;   ((NIL ;m
;     (1  ;p
;      #((0 1) (1 1) (0 1) (0 1)) ;b
;      NIL)))))) ;m

; game-tree: (turn board moves)
; moves: ( (move game-tree)* )

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (cons (list nil
                  (game-tree (add-new-dice board player (1- spare-dice))
                             (mod (1+ player) *num-players*)
                             0
                             t))
            moves)))

(defun game-tree (board player spare-dice first-move)
  "(Functional)"
  (list player
        board
        (add-passing-move board
                          player
                          spare-dice
                          first-move
                          (attacking-moves board player spare-dice))))

;(game-tree #((0 1) (1 1) (0 2) (1 1)) 0 0 t) ;=>

;(0                           ;player
; #((0 1) (1 1) (0 2) (1 1))  ;board
; (((2 3) (0                  ;moves
;          #((0 1) (1 1) (0 1) (0 1))
;          ((NIL (1
;                 #((0 1) (1 1) (0 1) (0 1))
;                 NIL)))))))

;;; Main loop

(defun winners (board)
  "(Functional)"
  (let* ((tally (loop for hex across board
                      collect (car hex)))
         (totals (mapcar (lambda (player)
                           (cons player (count player tally)))
                         (remove-duplicates tally)))
         (best (apply #'max (mapcar #'cdr totals))))
    (mapcar #'car
            (remove-if (lambda (x)
                         (not (eq (cdr x) best)))
                       totals))))

(defun announce-winner (board)
  (fresh-line)
  (let ((w (winners board)))
    (if (> (length w) 1)
        (format t "The game is a tie between ~a" (mapcar #'player-letter w))
        (format t "The winner is ~a" (player-letter (car w))))))

(defun handle-human (tree)
  "(Imperative)"
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (loop for move in moves
          for n from 1
          do (let ((action (car move)))
               (fresh-line)
               (format t "~a. " n)
               (if action
                   (format t "~a -> ~a" (car action) (cadr action))
                   (princ "end turn"))))
    (fresh-line)
    (cadr (nth (1- (read)) moves))))

(defun print-info (tree)
  "(Imperative)"
  (fresh-line)
  (format t "current player = ~a" (player-letter (car tree)))
  (draw-board (cadr tree)))

(defun play-vs-human (tree)
  "(Imperative)"
  (print-info tree)
  (if (caddr tree)
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

; (play-vs-human (game-tree (gen-board) 0 0 t))
