;;;; STREAMS

;; Change the SLIME directory, in order to write to where the exercise is:
;; , cd c:/Downloads/Information/Code/lisp-book-exercises/land_of_lisp/
;; Or start Emacs by editing one of the files in the LOL directory.

;;; Output streams
(output-stream-p *standard-output*)
(write-char #\x *standard-output*)

;;; Input streams
(input-stream-p *standard-input*)
(read-char *standard-input*)

;;; File handlers
(with-open-file (my-stream "data.txt" :direction :output)
  (print "my data" my-stream))

(with-open-file (my-stream "data.txt" :direction :input)
  (read my-stream))

(with-open-file (my-stream "data.txt" :direction :output
                                      :if-exists :error) ; Error on exist
  (print "my data" my-stream))

(with-open-file (my-stream "data.txt" :direction :output
                                      :if-exists :supersede) ;; Overwrite on exist
  (print "my data" my-stream))

;;; Data and files
(let ((animal-noises '((dog . woof)
                       (cat . meow))))
  (with-open-file (my-stream "animal-noises.txt" :direction :output)
    (print animal-noises my-stream)))

(with-open-file (my-stream "animal-noises.txt" :direction :input)
  (read my-stream))


;;; Sockets (CLISP)
(defun server()
  #+clisp
  (
   (defparameter my-socket (socket-server 4321))
   (defparameter my-stream (socket-accept my-socket)) ; blocking
   (read my-stream)
   (read)
   (print "What up, Client!" my-stream)
   (close my-stream)
   (socket-server-close my-socket)
   )
)

(defun client()
  #+clisp
  (
   (defparameter my-stream (socket-connect 4321 "127.0.0.1")) ;ON THE CLIENT
   (print "Yo Server!" my-stream)
   (read)
   (read my-stream)
   (close my-stream)
   )
)

;; CLISP installation
;; * CLISP installation instructions: http://www.jonathanfischer.net/modern-common-lisp-on-windows/
;; * CLISP + SLIME - http://pchristensen.com/blog/articles/installing-clisp-emacs-and-slime-on-windows-xp/
;; * SLIME + CLISP - https://github.com/slime/slime/blob/master/PROBLEMS

;; Error same as these:
;; * http://comments.gmane.org/gmane.lisp.slime.devel/9853
;; * https://groups.google.com/forum/#!msg/quicklisp/ysmLXm8js5c/dYXr5zKjXVkJ

;;; SBCL sockets

;; SBCL socket documentation: http://www.sbcl.org/manual/#Networking
;; SBCL socket examples: https://github.com/drichardson/examples/blob/master/lisp/sbcl-sockets.lisp

;;; CL-Cookbook sockets

;; CL-Cookbook: http://cl-cookbook.sourceforge.net/sockets.html
;; CLOCC - http://clocc.sourceforge.net/ 
;; PORT - http://clocc.sourceforge.net/dist/port.html 
;; CLLIB - http://clocc.sourceforge.net/dist/cllib.html 

;;; String streams
(defparameter foo (make-string-output-stream))
(princ "This will go into foo. " foo)
(princ "This will also go into foo. " foo)
(get-output-stream-string foo)

;; Send streams to functions, so you can debug more easily later.

;; Capturing output streams:
(with-output-to-string (*standard-output*)
  (princ "the sum of ")
  (princ 5)
  (princ " and ")
  (princ 2)
  (princ " is ")
  (princ (+ 2 5)))

