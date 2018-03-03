(defun bad-reverse (lst)
  "destructive reverse"
  (let* ((len (length lst))
         (ilimit (truncate (/ len 2))))
    (do ((i 0 (1+ i))         ; 0..ilimit-1
         (j (1- len) (1- j))) ; len-1..ilimit+1
        ((>= i ilimit))       ; end
      (rotatef (nth i lst) (nth j lst)))))

(defun good-reverse (lst)
  "nondestructive reverse"
  (labels ((rev (lst acc)
             (if (null lst)
                 acc
                 (rev (cdr lst) (cons (car lst) acc)))))
    (rev lst nil)))

(cons 'a '(b c))
(cons 'b 'c)
(cons 'a '(b c))
