;;; GRAPH UTILITIES

#|
(defparameter *wizard-nodes* '((living-room (you are in the living-room.
                                             a wizard is snoring loudly on the couch.))
                               (garden (you are in a beautiful garden.
                                        there is a well in front of you.))
                               (attic (you are in the attic. there
                                       is a giant welding torch in the corner.))))

(defparameter *wizard-edges* '((living-room (garden west door)
                                (attic upstairs ladder))
                               (garden (living-room east door))
                               (attic (living-room downstairs ladder))))
|#

(defun dot-name (exp)
  (substitute-if #\_
                 (complement #'alphanumericp)
                 (prin1-to-string exp))) ; prin1 - print on one line

(defparameter *max-label-length* 30)

(defun dot-label (exp)
  (if exp
      (let ((s (write-to-string exp :pretty nil)))
        (if (> (length s) *max-label-length*)
            (concatenate 'string (subseq s 0 (- *max-label-length* 3)) "...")
            s))
      ""))

;(dot-label '(a (b c) d (a (b c) d)(a (b c) d)(a (b c) d (a (b c) d (a (b c) d)))))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

; (nodes->dot *wizard-nodes*) ; =>
; LIVING_ROOM[label="(LIVING-ROOM (YOU ARE IN TH..."];
; GARDEN[label="(GARDEN (YOU ARE IN A BEAUT..."];
; ATTIC[label="(ATTIC (YOU ARE IN THE ATTI..."];

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

; (edges->dot *wizard-edges*) ; =>
; LIVING_ROOM->GARDEN[label="(WEST DOOR)"];
; LIVING_ROOM->ATTIC[label="(UPSTAIRS LADDER)"];
; GARDEN->LIVING_ROOM[label="(EAST DOOR)"];
; ATTIC->LIVING_ROOM[label="(DOWNSTAIRS LADDER)"];

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

; (graph->dot *wizard-nodes* *wizard-edges*)

(defun dot->png (fname thunk &optional (dir "C:/Downloads/Programs/graphviz-2.38/release/bin/"))
  (with-open-file (*standard-output*
                   (concatenate 'string dir fname)
                   :direction :output
                   :if-exists :supersede)
    (funcall thunk))
  ;;(ext:shell (concatenate 'string "dot -Tpng -O " fname)) ; Not in SBCL
  )

(defun graph->png (fname nodes edges)
  (dot->png fname ;Needs dot-png
            (lambda ()
              (graph->dot nodes edges))))

; (graph->png "wizard.dot" *wizard-nodes* *wizard-edges*)

;;; Undirected graph utilities

(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname
            (lambda ()
              (ugraph->dot nodes edges))))

; (ugraph->png "uwizard.dot" *wizard-nodes* *wizard-edges*)
