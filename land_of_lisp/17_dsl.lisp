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
  "My functional version of print-tag"
  (concatenate 'string
               '(#\<)
               (when closingp '(#\/))
               (string-downcase name)
               (format nil "~{~A~}"
                       (mapcar (lambda (att)
                                 (format nil " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
                               alst))
               '(#\>)))

(defun make-tag1 (name alst closingp)
  "My functional version of print-tag, but without '(#\<)s"
  (concatenate 'string
               "<"
               (when closingp "/")
               (string-downcase name)
               (format nil "~{~A~}"
                       (mapcar (lambda (att)
                                 (format nil " ~a=\"~a\"" (string-downcase (car att)) (cdr att)))
                               alst))
               ">")) ; Looks like this is equivalent to the above, probably because of how concat works.

;; (make-tag 'test '((color . blue)) nil)
;; (make-tag1 'test '((color . blue)) nil)

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

(defun ftag (name alist &rest rest)
  "My functional version of tag"
  ; (break) ;Debug for testing
  (concatenate 'string
               (make-tag name alist nil)
               (apply #'concatenate 'string rest)
               (make-tag name nil t)))

;; (ftag 'mytag '((color . blue)))
;; (ftag 'mytag '((color . blue)) (ttag 'newtag '((color . red))))
;; (ftag 'mytag '((color . blue)) (ttag 'newtag '((color . red))) (ttag 'newtag '((color . green))))

(defun ftag1 (name alist &rest rest)
  "ftag without apply"
  (concatenate 'string
               (make-tag name alist nil)
               `(concatenate 'string ,@rest) ;Doesn't work, not interpreted
               (make-tag name nil t)))

;; (ftag1 'mytag '((color . blue)) ())

(defun ftag2 (name alist &rest rest)
  "ftag without apply"
  `(concatenate 'string  ;Doesn't work, nothing is interpreted. Put this in an EVAL?
                (make-tag ',name ',alist nil)
                (concatenate 'string ,@rest) 
                (make-tag ',name nil t)))

;; (ftag2 'mytag '((color . blue)))
;; (ftag2 'mytag '((color . blue)) "testing" "123")
;; (eval (ftag2 'mytag '((color . blue)) "testing" "123"))

(defun ftag3 (name alist &rest rest)
  "ftag without apply"
  (eval ; Works! Is "(defun x () (eval" equivalent to "(defmacro x ()" ?
   `(concatenate 'string  
                 (make-tag ',name ',alist nil)
                 (concatenate 'string ,@rest) 
                 (make-tag ',name nil t))))

;; (ftag3 'mytag '((color . blue)))
;; (ftag3 'mytag '((color . blue)) "testing" "123")


;;; Generating HTML

;;(ftag 'html ()
;;      (ftag 'body ()
;;            "Hello World!"))

(defmacro svg (&body body)
  `(tag svg (xmlns "http://www.w3.org/2000/svg"
                   "xmlns:xlink" "http://www.w3.org/1999/xlink")
     ,@body))

(defun fsvg (&rest rest)
  (ftag 'svg '((xmlns "http://www.w3.org/2000/svg")
               ("xmlns:xlink" "http://www.w3.org/1999/xlink"))
        (apply #'concatenate 'string rest)))

;; (fsvg ())

(defun brightness (col amt)
  (mapcar (lambda (x)
            (min 255 (max 0 (+ x amt))))
          col))

;; (brightness '(255 0 255) -100)

(defun svg-style (color)
  (format nil
          "~{fill:rgb(~a,~a,~a);stroke:rgb(~a,~a,~a)~}"
          (append color
                  (brightness color -100))))

;; (svg-style '(255 0 255))

(defun circle (center radius color)
  (tag circle (cx (car center)
               cy (cdr center)
               r radius
               style (svg-style color))))

;; (svg (circle '(50 . 50) 50 '(255 0 0)) (circle '(100 . 100) 50 '(0 0 255)))

(defun fcircle (center radius color)
  (ftag 'circle `((cx ,@(car center)) ;Not sure why I need ,@ instead of , but if I don't I get (50)
                  (cy . ,(cdr center)) ; This works.
                  (r ,@radius) ; Somehow translates to (R . 50)
                  (style ,@(svg-style color)))))

;; (fcircle '(50 . 50) 50 '(255 0 0))
;; (fsvg (fcircle '(50 . 50) 50 '(255 0 0)) (fcircle '(100 . 100) 50 '(0 0 255)))
;; `(test 123) ;=> (test 123) ; Output as data, not executed as code.
;; '(test 123)

