(defun all-nicknames (names)
  "example of something that's more complicated than it needs to be."
  (if (null names)
      nil
      (nconc (nicknames (car names))
             (all-nicknames (cdr names)))))

;; Replaced by:
;; MAPCAN
;; Apply FUNCTION to successive tuples of elements of LIST and MORE-LISTS.
;; Return NCONC of FUNCTION return values.
(mapcan #'nicknames people)

;; Finding nearest bookshop town
(let ((town (find-if #'bookshops towns))) ; Throws away the town
  (values town (bookshops town))) 

;; Don't recompute

(defun find-books (towns)
  (if (null towns)
      nil
      (let ((shops (bookshops (car towns))))
        (if shops
            (values (car towns) shops)
            (find-books (cdr towns))))))

;; Replace with a utility

(defun find2 (fn lst)
  (if (null lst)
    nil
    (let ((val (funcall fn (car lst))))
      (if val
          (values (car lst) val)   ; Return find with result
          (find2 fn (cdr lst)))))) ; Or tail-recurse with the rest of the list

(find2 #'bookshops towns)

;; Abstraction
;; 1. Don't wire in behaviour.

