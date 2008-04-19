;; -*- Mode: Lisp; -*- 


;;;; Interface and toplevel definitions for the Fast Tiny Rule Engine 
;;;;  Modified: forbus on Tue Apr 2 10:16:22 1996

;;; Copyright (c) 1986-1992, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defstruct (ftre (:PRINT-FUNCTION ftre-printer))
  title
  (dbclass-table nil)
  (debugging nil)
  (debugging-contexts nil)
  (normal-queue nil)
  (asn-queue nil)
  (depth 0)
  (max-depth 5)
  (local-data nil)
  (local-rules nil)
  (rule-counter 0)
  (rules-run 0))

(defun ftre-printer (ftre st ignore)
  (format st "<FTRE: ~A>" (ftre-title ftre)))

(proclaim '(special *ftre*)) ;; Global default

(defvar *ftre* nil "Name for default FTRE")

(defmacro With-FTRE (tre &rest forms)
  `(let ((*ftre* ,tre)) ,@ forms))

(defun in-FTRE (tre) (setq *ftre* tre))

(defmacro debugging-ftre (msg &rest args)
  `(when (ftre-debugging *ftre*)
	 (format t ,msg ,@ args)))

(defmacro debugging-contexts (msg &rest args)
  `(when (ftre-debugging-contexts *ftre*)
	 (format t ,msg ,@ args)))

(defun create-ftre (title &key (debugging nil)
			  (debugging-contexts nil)
			  (max-depth 5))
  (make-ftre :TITLE title
	     :DBCLASS-TABLE (make-hash-table :test #'eq)
	     :DEBUGGING debugging
	     :DEBUGGING-CONTEXTS debugging-contexts
	     :MAX-DEPTH max-depth))

(defun debug-ftre (ftre debugging context?)
  (setf (ftre-debugging ftre) debugging)
  (setf (ftre-debugging-contexts ftre) context?))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Toplevel driver procedure

(defun run (*ftre*) 
    (format T "~%>>")
    (do ((form (read) (read)))
        ((member form '(quit stop exit abort)) nil)
        (format t "~%~A" (eval form))
        (run-rules *ftre*)
        (format t "~%>>")))

(defun run-forms (*ftre* forms)
  (dolist (form forms) 
    (with-ftre *ftre* (eval form))
    (run-rules *ftre*)))

(defun show (*ftre*) (show-data) (show-rules))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Evaluating code in a context

(defun try-in-context (assumption form
				  &optional (*ftre* *ftre*)
				  &aux (depth 0))
  (setq depth (ftre-depth *ftre*))
  (when (> depth (ftre-max-depth *ftre*))
	(debugging-contexts
	 "~% ~A(~D): Punting on trying ~A, too deep."
	 *ftre* (ftre-depth *ftre*) assumption)
     (return-from TRY-IN-CONTEXT nil))
  (let ((old-local-data (ftre-local-data *ftre*))
	(old-local-rules (ftre-local-rules *ftre*))
	(old-normal-queue (ftre-normal-queue *ftre*))
	(old-asn-queue (ftre-asn-queue *ftre*))
	(result nil))
    (setf (ftre-normal-queue *ftre*) nil)
    (setf (ftre-asn-queue *ftre*) nil)
    (incf (ftre-depth *ftre*))
    (push (ftre-depth *ftre*) (ftre-local-data *ftre*))
    (debugging-contexts
     "~% ~A(~D): Trying ~A."
     *ftre* (ftre-depth *ftre*) assumption)
    (with-ftre *ftre*
     (if assumption (assert! assumption))
     (run-rules *ftre*)
     (debugging-contexts
      "~% ~A(~D): Context ~A for ~A."
      *ftre* (ftre-depth *ftre*) assumption form)
     (debugging-contexts
      "~%      ~D facts and ~D rules in local context."
      (- (length (ftre-local-data *ftre*))
	 (length old-local-data))
      (- (length (ftre-local-rules *ftre*))
	 (length old-local-rules)))
      (setq result (eval form))
      (setf (ftre-local-data *ftre*) old-local-data)
      (setf (ftre-local-rules *ftre*) old-local-rules)
      (setf (ftre-normal-queue *ftre*) old-normal-queue)
      (setf (ftre-asn-queue *ftre*) old-asn-queue)
      (decf (ftre-depth *ftre*))
      result)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Context introduction mechanism
;; Exploring an assumption requires pushing a context.
;; Each context inherits all facts and rules from those
;; above it.  Facts and rules added inside a context
;; are flushed when that context is left.

(defun seek-in-context (assumption goal
			   &optional (*ftre* *ftre*))
  (let ((depth (ftre-depth *ftre*)))
    (when (null goal)
	  (error "Seek-in-context requires a goal.")) 
    (when (> depth (ftre-max-depth *ftre*))
	  (debugging-contexts 
	   "~% ~A(~D): Punting on assuming ~A;"
	   *ftre* depth assumption )
	  (debugging-contexts 
	   "seeking ~A, resource limits exceeded"
	   goal)
	  (return-from SEEK-IN-CONTEXT nil))
    (let ((old-local-rules (ftre-local-rules *ftre*))
	  (old-local-data (ftre-local-data *ftre*))
	  (old-normal-queue (ftre-normal-queue *ftre*))
	  (old-asn-queue (ftre-asn-queue *ftre*))
	  (result nil))
      (setf (ftre-normal-queue *ftre*) nil)
      (setf (ftre-asn-queue *ftre*) nil)
      (incf (ftre-depth *ftre*))
      (push (ftre-depth *ftre*) (ftre-local-data *ftre*))
      (debugging-contexts
       "~% ~A(~D): Assuming ~A; seeking ~A."
       *ftre* (ftre-depth *ftre*) assumption goal)
      (if assumption (assert! assumption *ftre*))
      (with-ftre *ftre*
	 (assert! `(show ,goal))
	 (eval
	  `(rule (,goal)
		 (when (= (ftre-depth *ftre*)
			  ,(ftre-depth *ftre*))
		  (debugging-contexts
		   "~% (~D): Found goal ~A!"
		   *ftre* (ftre-depth *ftre*) ',goal)
		  (throw 'punt-context t)))))
      (catch 'punt-context (run-rules *ftre*))
      (debugging-contexts
	"~% ~A(~D): Retracting ~A, sought ~A."
	*ftre* (ftre-depth *ftre*) assumption goal)
      (debugging-contexts
	"~%  ~A~%  ~D facts and ~D rules in local context."
	(if (fetch goal) "Succeeded!" "Failed...")
	(- (length (ftre-local-data *ftre*))
	   (length old-local-data))
	(- (length (ftre-local-rules *ftre*))
	   (length old-local-rules)))
      (setq result (fetch goal))
      (decf (ftre-depth *ftre*))
      (setf (ftre-local-data *ftre*) old-local-data)
      (setf (ftre-local-rules *ftre*) old-local-rules)
      (setf (ftre-normal-queue *ftre*) old-normal-queue)
      (setf (ftre-asn-queue *ftre*) old-asn-queue)
      result)))
