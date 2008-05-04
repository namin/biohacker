;; -*- Mode: Lisp; -*- 

;;;; LTRE definitions  
;;;; Last edited 1/29/93, by KDF

;;; Copyright 1986, 1989, 1990, 1991 Kenneth D. Forbus, 
;;; Nortwestern University, and Johan de Kleer, Xerox Corporation.
;;; All Rights Reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defstruct (ltre (:PRINT-FUNCTION ltre-print-procedure))
  title                   ; Pretty name
  ltms                    ; Pointer to its LTMS
  (dbclass-table nil)       ; Hash table of dbclasses
  (datum-counter 0)       ; Unique ID for asserts
  (rule-counter 0)        ; Unique ID for rules
  (debugging nil)         ; Show basic operations
  (queue nil)             ; Queue for rules
  (rules-run 0))          ; Statistics

;; The RULES field of LTREs has been eliminated, since the same
;; information can be reconstructed from the dbclass tables.

(defun ltre-print-procedure (l st ignore)
  (declare (ignore ignore))
  (format st "<LTRE: ~A>" (ltre-title l)))

(defvar *LTRE* nil) ;; Default LTRE
;;; The binding of this symbol is used inside rules and various
;;; macros to specify which LTRE a rule or fact should be stored in.
;;; The next few procedures encapsulate this choice

(defmacro with-LTRE (ltre &rest forms)
  `(let ((*LTRE* ,ltre)) ,@ forms))

(defun In-LTRE (ltre) (setq *LTRE* ltre)) ;; Analogy with in-package

(defmacro debugging-ltre (msg &rest args)
  `(when (ltre-debugging *LTRE*) (format t ,msg  ,@args)))

(defun create-ltre (title &key debugging)
   (let ((l (make-ltre :TITLE title 
               :LTMS (create-ltms (list :LTMS-OF title) 
                        :NODE-STRING 'make-node-string
                        :CACHE-DATUMS? nil)
	   :DBCLASS-TABLE (make-hash-table)
	   :DEBUGGING debugging)))
   (change-ltms (ltre-ltms l)
		:ENQUEUE-PROCEDURE
		#'(lambda (pair) (enqueue pair l)))
   (setq *LTRE* l)))

(defun change-ltre (ltre &key (debugging nil debugging?))
  (if debugging? (setf (ltre-debugging ltre) debugging)))

;;;; Running LTRE

(defun uassert! (fact &optional (just 'user) (*ltre* *ltre*))
  (assert! fact just) ;; Do internal operation
  (run-rules *ltre*))        ;; Run the rules

(defun uassume! (fact reason &optional (*ltre* *ltre*)) 
  (assume! fact reason)
  (run-rules *ltre*))

(defun run-forms (forms &optional (*LTRE* *LTRE*))
  (dolist (form forms) (eval form) (run-rules *ltre*)))

(defun run (&optional (*LTRE* *LTRE*)) ;; Toplevel driver function
    (format T "~%>>")
    (do ((form (read) (read)))
        ((member form '(quit stop exit abort)) nil)
        (format t "~%~A" (eval form))
        (run-rules)
        (format t "~%>>")))

(defun show (&optional (*LTRE* *LTRE*) (stream *standard-output*))
  (format stream "For LTRE ~A:" (ltre-title *LTRE*)) 
  (show-data *LTRE* stream) (show-rules *LTRE* stream))

;;;; Some debugging stuff

(defun show-by-informant (informant &optional (*LTRE* *LTRE*)
				    &aux (count 0))
  (dolist (clause (ltms-clauses (ltre-ltms *LTRE*)) count)
	  (when (if (listp (clause-informant clause))
		    (eq (third (clause-informant clause)) informant)
		  (eq (clause-informant clause) informant))
		(incf count)
		(pprint (view-clause clause)))))

(defun view-clause (cl)
  (cons 'OR (mapcar #'(lambda (x)
			 (if (eq (cdr x) :FALSE) `(NOT ,(view-node (car x)))
			   (view-node (car x))))
		    (clause-literals cl))))
  