;; -*- Lisp -*-

;;;; ATMS version of the N-queens problem

;; Copyright (c) 1986, 1987, 1988, 1989, 1990 Kenneth D. Forbus, 
;;   Northwestern University, and Johan de Kleer, Xerox Corporation.  
;; All rights reserved.

(in-package 'user)

;; This version uses special-purpose lisp code for simplicity.
;; Clearly one queen per column is required, hence choice sets
;; are placement of queen within the rows for each column.

(defvar *queen-nodes* nil)
(defvar *solutions* nil)
(defvar *atms*)

(defun n-queens (n)
  (setq *atms* (create-atms "N-queens"))
  (setq *solutions* nil)
  (setq *queen-nodes* nil)
  (setup-queen-nodes n)
  (setq *solutions* (interpretations *atms* *queen-nodes*))
  (length *solutions*))

(defun setup-queen-nodes (n)
  (do ((i 1 (1+ i))
       (column nil nil)
       (nodes nil))
      ((> i n)
       (setq nodes (apply #'append *queen-nodes*))
       (dolist (n1 nodes)
	 (dolist (n2 nodes)
	   (unless (or (eq n1 n2)
		       (= (caddr (tms-node-datum n1))
			  (caddr (tms-node-datum n2))))
	     (when (queens-capture? (tms-node-datum n1)
			            (tms-node-datum n2))
	       (nogood-nodes 'QUEENS-CAPTURE (list n1 n2)))))))
    (do ((j 1 (1+ j)))
	((> j n) (push column *queen-nodes*))
      (push (tms-create-node *atms* `(Queen ,j ,i) :assumptionp T) column))))

(defun queens-capture? (qa1 qa2)
  (or (= (cadr qa1) (cadr qa2))
      (= (abs (- (cadr qa1) (cadr qa2)))
	 (abs (- (caddr qa1) (caddr qa2))))))

(defun test-queens (from to)
  (do ((n from (1+ n)))
      ((> n to))
    (time (n-queens n))
    (format t "~%For ~D queens, ~D solutions."
	    n (length *solutions*))))
