(defun imp (x)
  (let (y sqr)
    (setq y (car x))
    (setq sqr (expt y 2))
    (list 'a sqr)))

(imp '(10 20))

;; Turn the code inside out.
;; Start with the last call, put it down first.

(defun fun (x)
  (list 'a (expt (car x) 2)))

(fun '(10 20))
