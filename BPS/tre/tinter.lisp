;; -*- Mode: Lisp; -*-

;;;; TRE interface
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;; Includes user hooks (for people and programs).

(defstruct (tre (:PRINT-FUNCTION tre-printer))
  title                     ; String for printing
  (dbclass-table nil)         ; symbols --> classes
  (debugging nil)           ; prints extra info if non-nil
  (queue nil)               ; LIFO
  (rule-counter 0)          ; Unique id for rules
  (rules-run 0))            ; Statistics

(defun tre-printer (tre st ignore)
  (format st "<TRE: ~A>" (tre-title tre)))

(proclaim '(special *TRE*)) ;; Current TRE

(defvar *TRE* nil "Name for default TRE")

(defmacro With-TRE (tre &rest forms)
  `(let ((*TRE* ,tre)) ,@ forms))

(defun in-TRE (tre) (setq *TRE* tre))

(defmacro debugging-tre (msg &rest args)
  `(when (tre-debugging *TRE*) (format t ,msg ,@ args)))


(defun create-tre (title &key debugging)
  (make-tre :TITLE title
	    :DBCLASS-TABLE (make-hash-table :test #'eq)
	    :DEBUGGING debugging))

(defun debug-tre (tre debugging)
  (setf (tre-debugging tre) debugging))

;;;; Drivers for programs and people  

(defun run (&optional (*TRE* *TRE*))
    (format T "~%>>")
    (do ((form (read) (read)))
        ((member form '(quit stop exit)) nil)
        (format t "~%~A" (eval form))
        (run-rules *tre*)  ;; Defined in RULES module
        (format t "~%>>")))

(defun run-forms (*TRE* forms) ;; Toplevel for programs
  (dolist (form forms)
	  (eval form) (run-rules *TRE*)))

(defun show (&optional (stream *standard-output*))
  ;; Pass on the request to both modules of default TRE
  (show-data stream)
  (show-rules stream))
