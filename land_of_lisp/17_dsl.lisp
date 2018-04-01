;;;; MAKING A DOMAIN SPECIFIC LANGUAGE

(defun print-tag (name alst closingp)
  (princ #\<)
  (when closingp
    (princ #\/))
  (princ (string-downcase name))
  (mapc (lambda (att)
          (format t " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
        alst)
  (princ #\>))

(defun make-tag (name alst closingp)
  "My functional version of print-tag that returns tag as a string"
  (concatenate 'string
               '(#\<)
               (when closingp '(#\/))
               (string-downcase name)
               (format nil "~{~A~}"
                       (mapcar (lambda (att)
                                 (format nil " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
                               alst))
               '(#\>)))

;; So it looks like these print " but return "\"" as string representations.
;; (format nil "\"")
;; (princ "\"")

;; Okay, I see why he's printing. Trying to justify using a macro.

(defmacro tag (name atts &body body)
  `(progn (print-tag ',name
                     (list ,@(mapcar (lambda (x)
                                       `(cons ',(car x) ,(cdr x)))
                                     (pairs atts)))
                     nil)
          ,@body
          (print-tag ',name nil t)))

;; Example:
;; (tag mytag (color 'blue height (+ 4 5)))

;; Nested example:
;; (tag mytag (color 'blue height (+ 4 5)) (tag newtag (color 'red height 3)))

(defun ftag (name alist &optional inner)
  "My functional version of tag"
  (concatenate 'string
               (make-tag name alist nil)
               inner
               (make-tag name nil t)))

;; (ftag 'mytag '((color . blue)) (ttag 'newtag '((color . red))))


