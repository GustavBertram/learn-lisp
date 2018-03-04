;; 1. Side effects are harmless if they modify something that no one else owns.

(defun harmless (expr)
  "use of destructive function in nondestructive function"
  (nconc (copy-list expr) (list 'maybe)))

;; 2. A given invocation can safely modify what it uniquely owns. Talk about ownership
;;    in invocations, not functions:

(let ((x 0))
  "Function modifies x, but invocation of this function doen't."
  (defun total (y)
    (incf x y)))

(total 10)

;; 3. An invocation owns: Objects it receives as return values, but not objects passed to it as arguments.

(defun ok (x)
  "nondestructive"
  (nconc (list 'a x) (list 'c)))

(defun not-ok (x)
  "destructive"
  (nconc (list 'a) x (list 'c)))

(defparameter a '(1 2 3))
(not-ok a) ; modifies a

;; 4. Functions can't share objects with other code that doesn't follow the rules.

(defun anything (x)
  (+ x *anything*))

;; Code that follows these rules is almost as good as purely functional code.
;; It presents a *functional interface* to the world.
;; If you call it twice with the same arguments, you get the same result.

(defun f (x)
  "Not safe if g is identity"
  (let ((val (g x)))
     ; safe to modify val here?
    ))

;; 5. When looking at function safety, check the functions it calls.
;; 6. Don't return anything that isn't safe to modify.

(defun unsafe-exclaim (expression)
  "returns unsafe quotes"
  (append expression '(oh my)))

(unsafe-exclaim '(lions and tigers and bears))

(nconc * '(goodness))

(unsafe-exclaim '(fixnums and bignums and floats))

(defun safe-exclaim (expression)
  "returns safe expression"
  (append expression (list 'oh 'my)))

;; 7. Macro expanders can safely incorporate quoted lists in the expansions they generate,
;;    if the expansions go straight to the compiler.


