;; Conrad's rule of thumb for comparing stuff:
;; 1. Use EQ to compare symbols.
;; 2. Use EQUAL for everything else.

;; EQ can compare symbols and conses.
(eq 'a 'a) ;=> T

(eq "a" "a") ;=> NIL
(eq 5 5) ;=> Undefined by the standard.

;; EQUAL returns true when things are isomorphic, or look the same.

(equal "a" "a") ;=> T
(equal (list 1 2 3) (list 1 2 3))
(equal 5 5)

(equal 5 5.0) ;=> NIL

;; EQL is similar to EQ but also handles numbers and characters.
(eql 5 5) ;=> T
(eql #\a #\a)

;; EQUALP can compare strings with different capitalization, and different types of numbers.
(equalp 5 5.0) ;=> T
(equalp "ABBA" "AbbA")

;; = handles numbers.
(= 5 5.0) ;=> T

;; STRING-EQUAL handles strings.

;; CHAR-EQUAL handles chars.
