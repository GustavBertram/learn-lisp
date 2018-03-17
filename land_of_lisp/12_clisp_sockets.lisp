;;;; CLISP Sockets example
(defun server()
  (defparameter my-socket (socket-server 4444))
  (defparameter my-stream (socket-accept my-socket)) ; blocking
  (read my-stream)
  (print "What up, Client!" my-stream)
  (close my-stream)
  (socket-server-close my-socket))

(defun client()
  (defparameter my-stream (socket-connect 4444 "127.0.0.1")) ;ON THE CLIENT
  (print "Yo Server!" my-stream)
  (read my-stream)
  (close my-stream))
