(defparameter cxr
  '(("Func" "Sound" "Alt")
    ("----" "-----" "---")
    ("CAR" "kar" "FIRST")
    ("CDR" "cou-der" "REST")
    ("CAAR" "ka-ar")
    ("CADR" "kae-der" "SECOND")
    ("CDAR" "cou-dar")
    ("CDDR" "cou-dih-der")
    ("CAAAR" "ka-a-ar")
    ("CAADR" "ka-ae-der")
    ("CADAR" "ka-dar")
    ("CADDR" "ka-dih-der" "THIRD")
    ("CDAAR" "cou-da-ar")
    ("CDADR" "cou-dae-der")
    ("CDDAR" "cou-dih-dar")
    ("CDDDR" "cou-did-dih-der")
    ("CADDDR" "ka-dih-dih-der" "FOURTH")))

(defun table (tab)
  (flet ((row (r)
           (format t "~a~8t~a~16t~a~32t~%"
                   (or (third r) "")
                   (first r)
                   (second r)
                   )))
    (dolist (r tab)
      (row r))))

;; CAR/CDR Pronunciation Guide

;; Alt     Func    Sound           
;; ---     ----    -----           
;; FIRST   CAR     kar             
;; REST    CDR     cou-der         
;;         CAAR    ka-ar           
;; SECOND  CADR    kae-der         
;;         CDAR    cou-dar         
;;         CDDR    cou-dih-der     
;;         CAAAR   ka-a-ar         
;;         CAADR   ka-ae-der       
;;         CADAR   ka-dar          
;; THIRD   CADDR   ka-dih-der      
;;         CDAAR   cou-da-ar       
;;         CDADR   cou-dae-der     
;;         CDDAR   cou-dih-dar     
;;         CDDDR   cou-did-dih-der 
;; FOURTH  CADDDR  ka-dih-dih-der 

