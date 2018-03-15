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

;; LOL uses CLISP

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

;; CL-Cookbook: http://cl-cookbook.sourceforge.net/sockets.html
;; CLOCC - http://clocc.sourceforge.net/ 
;; PORT - http://clocc.sourceforge.net/dist/port.html 
;; CLLIB - http://clocc.sourceforge.net/dist/cllib.html 

