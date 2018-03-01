(defun make-dbms (db)
  "Return three closures for a db."
  (list
   #'(lambda (key)
       (cdr (assoc key db)))
   #'(lambda (key val)
       (push (cons key val) db)
       key)
   #'(lambda (key)
       (setf db (delete key db :key #'car))
       key)))

(setq cities (make-dbms '((boston . us) (paris . france))))

(funcall (first cities) 'boston)

(cdr (assoc 'test '((test . xyz) (moo . zzz))))

(remove 'test '((a . b) (aaa . a) (c . a) (test . moo)) :key #'car)
