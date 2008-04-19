;; -*- Mode: Lisp; -*-

;;;; Fast Tiny Rule Engine.
;;;;  Modified: forbus on Tue Apr 2 10:14:11 1996

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defvar *ftre-path* nil "FTRE's path")
(defvar *ftre-files* nil "FTRE's files")
(defvar *fqueen-rule-file* nil "FTRE's version of the n-queens rule")

(setq *ftre-path* (make-bps-path "ftre"))

(setq *ftre-files*
      '("finter"    ;; Interface
        "fdata"     ;; Database
        "frules"    ;; Rule system
        "unify"     ;; Unifier
        "funify"    ;; Open-coding for unification
        "fnd-ex"    ;; Natural deduction examples for ftre
        "fqueens"   ;; n-queens setup for FTRE
        ))

(setf *fqueen-rule-file* "fqrule")

(defun load-ftre (&key (action :compile-if-newer))
  (bps-load-files *ftre-path* *ftre-files* :action action))