;;;; Errors

(error "foo")

(define-condition foo () ()
  (:report (lambda (condition stream)
             (princ "Stop FOOing around, numbskull!" stream))))
(error 'foo)

;; Intercepting conditions

(defun bad-function ()
  (error 'foo))

(handler-case (bad-function)
  (foo () "somebody signaled foo!")
  (bar () "somebody signaled bar!"))

(unwind-protect (/ 1 0)
  (princ "I need to say 'flubyduby' no matter what"))




