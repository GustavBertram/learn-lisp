;;;; UTILITIES
;;; Collects the utilities in Let over Lambda Chapter 1

;;; MKSTR-SYMB

;; Make a symbol into a string
(defun mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) (princ a s))))

;; Intern symbols
(defun symb (&rest args)
  (values (intern (apply #'mkstr args))))

;;; GROUP

(defun group (source n)
  (if (zerop n) (error "zero length"))
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source)))
               (if (consp rest)
                   (rec rest (cons
                              (subseq source 0 n)
                              acc))
                   (nreverse
                    (cons source acc))))))
    (if source (rec source nil) nil)))

;;; FLATTEN

(defun flatten (x)
  (labels ((rec (x acc)
             (cond ((null x) acc)
                   ((atom x) (cons x acc))
                   (t (rec
                       (car x)
                       (rec (cdr x) acc))))))
    (rec x nil)))

;;; FACT-AND-CHOOSE

(defun factorial (x)
  "Return the factorial of x"
  (if (< x 0) (error "Can't get the FACTORIAL of a negative number")
      (if (= x 0)
          1
          (* x (factorial (1- x))))))

(defun choose (n k)
  "N choose K"
  (if (< n k) (error "Can't CHOOSE more K than available in N")
      (/ (factorial n)
         (factorial (- n k))
         (factorial k))))
