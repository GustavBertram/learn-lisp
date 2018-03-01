(defparameter *small* 1)
(defparameter *big* 100)

(defun guess-my-number ()
  "Let the computer guess your number."
  (ash (+ *big* *small*) -1))

(defun bigger ()
  "The number is bigger than the computer thinks"
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

(defun smaller ()
  "The number is smaller than the computer thinks"
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun start-over ()
  "Resets the game"
  (defparameter *small* 1)
  (defparameter *big* 100)
  (guess-my-number))
