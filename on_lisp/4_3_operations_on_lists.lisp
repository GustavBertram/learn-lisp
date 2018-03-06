;; Shortest idioms are not always the most efficient ones

(> (length x) (length y)) ; Iterates all the way through both to get lengths

(mapcar fn (append x y z)) ; Appending the lists takes time

;; Small list utilities

(proclaim '(inline last1 single append1 conc1 mklist))

(defun last1 (lst)
  "Returns last element, not last cons"
  (car (last lst)))

(defun single (lst)
  "Returns true if the list is a single element long, but more efficient that (= 1 (length lst))"
  (and (consp lst) (not (cdr lst))))

(defun append1 (lst obj)
  "Appends an object to a list, instead of two lists"
  (append lst (list obj)))

(defun conc1 (lst obj)
  "Concatenates an object to a list, instead of two lists"
  (nconc lst (list obj)))

(defun mklist (obj)
  "Make an object a list, but leave a list a list"
  (if (listp obj) obj (list obj)))

;; Longer utilities ------------------------------

(defun longer (x y)
  "Return T if x is longer that y. More efficient than (> (length x) (length y))."
  (labels ((compare (x y)
             (and (consp x)
                  (or (null y)
                      (compare (cdr x) (cdr y))))))
    (if (and (listp x) (listp y))
        (compare x y)
        (> (length x) (length y)))))

;; "FILTER is to SOME, what REMOVE-IF-NOT is to FIND-IF."
;;; FIND-IF returns first matching element, REMOVE-IF-NOT returns ALL matching elements.
;;; SOME returns first non-nil. ***FILTER returns ALL non-nil.***

(defun filter (fn lst)
  "Apply fn to every element of lst, and collects the non-nil ones."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (if val (push val acc)))) ; The combination of PUSH and NREVERSE
    (nreverse acc)))              ; is the standard idiom for accumulating a list

(filter #'(lambda (x) (if (numberp x) (1+ x)))
        '(a 1 2 b 3 c d 4))

; GROUP

(defun group (source n)
  "Group SOURCE into N size sublists"
  (if (zerop n) (error "zero length"))
  
  (labels ((rec (source acc)
             (let ((rest (nthcdr n source))) ; NTHCDR - perform CDR n times on a list.
               (if (consp rest) ; If there's any left
                   (rec rest (cons (subseq source 0 n) acc)) ;SUBSEQ - return copy of subsequence
                   (nreverse (cons source acc)))))) ;return acc
    (if source (rec source nil) nil)))

(nthcdr 2 '(1 2 3 4 5 6)) ; => (3 4 5 6)
(cons '(a) '(b c)) ; => ((A) B C)

;; Doubly recursive list utilities

(defun flatten (x)
  "Makes a list with sublists into a single flat list."
  (labels ((rec (x acc depth call)
             (format t "X: ~a ACC: ~a DEPTH: ~a CALL: ~a ~%" x acc depth call)
             (cond ((null x) acc) ; Return the result
                   ((atom x) (cons x acc)) ; Add atoms
                   (t (rec (car x) (rec (cdr x) acc (1+ depth) "A") (1+ depth) "B"))))) ; Else (REC (REC ACC))
    (rec x nil 0 "C")))

(flatten '(a (b c) ((d e) f)))

; TODO: Read these functions!
(defun prune (test tree)
  "Recurse into sublists and prune according to TEST"
  (labels ((rec (tree acc)
             (cond ((null tree) (nreverse acc))
                   ((consp (car tree))
                    (rec (cdr tree)
                         (cons (rec (car tree) nil) acc)))
                   (t (rec (cdr tree)
                           (if (funcall test (car tree))
                               acc
                               (cons (car tree) acc)))))))
    (rec tree nil)))
