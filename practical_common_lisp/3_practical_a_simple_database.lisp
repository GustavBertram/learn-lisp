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
