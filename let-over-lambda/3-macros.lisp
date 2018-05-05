;;;; MACRO BASICS
;; Contains code from Let Over Lambda, Chapter 3.

;;; Domain specific languages --------------------------------------------------

(sleep 180)

(sleep (* 3 60))

(defun sleep-minutes (m)
  (sleep (* m 60)))

(defun sleep-units% (value unit)
  (sleep
    (* value
       (case unit
         ((s) 1)
         ((m) 60)
         ((h) 3600)
         ((d) 86400)
         ((ms) 1/1000)
         ((us) 1/1000000)))))

(defmacro sleep-units (value unit)
  `(sleep
     (* ,value
        ,(case unit
           ((s) 1)
           ((m) 60)
           ((h) 3600)
           ((d) 86400)
           ((ms) 1/1000)
           ((us) 1/1000000)))))

(sleep-units 5 s)

(defmacro unit-of-time (value unit)
  `(* ,value
      ,(case unit
         ((s) 1)
         ((m) 60)
         ((h) 3600)
         ((d) 86400)
         ((ms) 1/1000)
         ((us) 1/1000000))))

(unit-of-time 1 d)

;;; Control structures ---------------------------------------------------------


