(defun make-cd (title artist rating ripped)
  "Create a CD record."
  (list :title title :artist artist :rating rating :ripped ripped))

(defvar *db* nil)

(defun add-record (cd)
  "Add a cd record to the db."
  (push cd *db*))

(defun dump-db ()
  "Dump the db."
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))

(defun dump-db1 ()
  "Alternate DB dump."
  (format t "~{~{~a:~10t~a~%~}~%~}" *db*))

(defun prompt-read (prompt)
  "Read typed response to a prompt."
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*))

(defun prompt-for-cd ()
  "Make a CD from interactive prompts."
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (or (parse-integer (prompt-read "Rating") :junk-allowed t) 0)
   (y-or-n-p "Ripped [y/n]")))

(defun add-cds ()
  "Add a bunch of CDs."
  (loop (add-record (prompt-for-cd))
        (if (not (y-or-n-p "Another? [y/n]: ")) (return))))

(defun save-db (&optional (filename "~/pcl_db.lisp"))
  "Save the DB."
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out))))

(defun load-db (&optional (filename "~/pcl_db.lisp"))
  "Load the DB."
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in)))))

;; Selectors

(defun select (selector-fn)
  (remove-if-not selector-fn *db*))

(defun artist-selector (artist)
  (lambda (cd) (equal (getf cd :artist) artist)))

(defun select-by-artist (artist)
  (select (artist-selector artist)))


(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda (cd)
      (and
       (if title (equal (getf cd :title) title) t)
       (if artist (equal (getf cd :artist) artist) t)
       (if rating (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t))))

;; Update

(defun update (selector-fn &key title artist rating (ripped nil ripped-p))
  (setf *db*
        (mapcar
         #'(lambda (row)
             (when (funcall selector-fn row)
               (if title (setf (getf row :title) title))
               (if artist (setf (getf row :artist) artist))
               (if rating (setf (getf row :rating) rating))
               (if ripped-p (setf (getf row :ripped) ripped)))
             row) *db*)))

(defun delete-rows (selector-fn)
  (setf *db* (remove-if selector-fn *db*)))

;; Macros

(defun make-comparison (field value)
  `(equal (getf cd ,field) ,value))

(defun make-comparisons-list (fields)
  (loop while fields
        collecting (make-comparison (pop fields) (pop fields))))

(defmacro nwhere (&rest clauses)
  `#'(lambda (cd) (and ,@(make-comparisons-list clauses))))
  
(macroexpand-1 '(where :title "Give Us a Break" :ripped t))
