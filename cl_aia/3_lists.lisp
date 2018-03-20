;;; Hierarchy of types
; list
; number
    ; integer
    ; float
        ; short-float
        ; single-float

;; 3.2
(12 32) ;=> 12 is not a function name

;; 3.3
( ;=> keeps listening for input
 )

;; 3.4
'())) ;=> An object cannot start with #\)

;; 3.5
'(1 2
  (3 4 
   (5 6))) ;=> (1 2 (3 4 (5 6)))

;; 3.6
; Lots of backspaces, or go to beginning of line and C-k a couple of times

;; 3.7
' ( 1 ) ;=> (1)
'       ( 1  2 3       ) ;=> (1 2 3)
