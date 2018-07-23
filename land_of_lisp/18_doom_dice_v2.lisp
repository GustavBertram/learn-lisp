;;;; DICE OF DOOM V2
;;; Contains code from Land of Lisp Ch 18.
;;; Updating Dice of Doom with lazy evaluation.

(load "15_doom_dice.lisp")
(load "18_lazy_evaluation.lisp")

;;; Bigger board
(defparameter *board-size* 4)
(defparameter *board-hexnum* (* *board-size* *board-size*))

;;; Redefine game functions using lazy library

;; Note: compare to 15_doom_dice.lisp side-by-side

(defun add-passing-move (board player spare-dice first-move moves)
  (if first-move
      moves
      (lazy-cons (list nil
                       (game-tree (add-new-dice board player (1- spare-dice))
                                  (mod (1+ player) *num-players*)
                                  0
                                  t))
                 moves)))

(defun attacking-moves (board cur-player spare-dice)
  (labels ((player (pos)
             (car (aref board pos)))
           (dice (pos)
             (cadr (aref board pos))))
    (lazy-mapcan
     (lambda (src)
       (if (eq (player src) cur-player)
           (lazy-mapcan
            (lambda (dst)
              (if (and (not (eq (player dst) cur-player))
                       (> (dice src) (dice dst)))
                  (make-lazy
                   (list (list (list src dst)
                               (game-tree (board-attack board cur-player src dst (dice src))
                                          cur-player
                                          (+ spare-dice (dice dst))
                                          nil))))
                  (lazy-nil)))
            (make-lazy (neighbors src)))
           (lazy-nil)))
     (make-lazy (loop for n below *board-hexnum*
                      collect n)))))

(defun handle-human (tree)
  "(Imperative)"
  (fresh-line)
  (princ "choose your move:")
  (let ((moves (caddr tree)))
    (labels ((print-moves (moves n)
               (unless (lazy-null moves)
                 (let* ((move (lazy-car moves))
                        (action (car move)))
                   (fresh-line)
                   (format t "~a. " n)
                   (if action
                       (format t "~a -> ~a" (car action) (cadr action))
                       (princ "end turn")))
                 (print-moves (lazy-cdr moves) (1+ n)))))
      (print-moves moves 1))
    (fresh-line)
    (cadr (lazy-nth (1- (read)) moves))))

(defun play-vs-human (tree)
  "(Imperative)"
  (print-info tree)
  (if (not (lazy-null (caddr tree)))
      (play-vs-human (handle-human tree))
      (announce-winner (cadr tree))))

