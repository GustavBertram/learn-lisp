;; Symmetry of nil

(equal nil 'nil)
(equal nil ())
(equal nil '())

;; List eater

(defun my-length (lst)
  (if lst
      (1+ (my-length (cdr lst)))
      0))

(my-length '(a b c d))
   
;; Member function returns the rest of the list.

(member nil '(a b nil c))
