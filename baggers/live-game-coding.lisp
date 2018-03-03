;; From: https://www.youtube.com/watch?v=6pMyhrDcMzw

(defun update-swank ()
  "Called from within the main loop, this keep the lisp repl 
   working while cepl runs"
  (continuable
   (let ((connection (or swank::*emacs-connection*
                         (swank::default-connection))))
     (when connection
       (swank::handle-requests connection t)))))

(defmacro continuable (&body body)
  "Helper macro that we can use to allow us to continue from an 
   error. Remember to hit C in slime or pick the restart so 
   errors don't kill the app."
  `(restart-case
       (progn ,@body)
     (continue () :report "Continue")))
