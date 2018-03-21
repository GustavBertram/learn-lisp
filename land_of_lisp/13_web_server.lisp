;;;; WEB SERVER
;;; (CLISP project)

;;; Decoding request parameter values

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
               (coerce (list c1 c2) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  (labels ((f (lst)
             (when lst
               (case (car lst)
                 (#\% (cons (http-char (cadr lst) (caddr lst))
                            (f (cdddr lst))))
                 (#\+ (cons #\space (f (cdr lst))))
                 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

;(decode-param "foo") ;=> "foo"
;(decode-param "foo%3F") ;=> "foo?"
;(decode-param "foo+bar") ;=> "foo bar"

;;; Decoding request parameters

(defun parse-params (s)
  (let* ((i1 (position #\= s))
         (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

; (parse-params "name=bob&age=25&gender=male") ;=> ((NAME . "bob") (AGE . "25") (GENDER . "male"))



