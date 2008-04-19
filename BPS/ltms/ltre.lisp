;;;;  Modified: everett on Mon Mar  6 13:09:00 1995

;;;; LTRE -- a version of TRE which uses the LTMS
;;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1989, 1990, 1991 Kenneth D. Forbus, Northwestern University,
;;; Johan de Kleer and Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defvar *ltre-path* (make-bps-path "ltms"))
 

;; Unless the ltre is placed in its own package, unexporting won't
;; do the trick, we have to unequivocally destroy rlet
#+MCL (unintern 'rlet (find-package :ccl))


(defparameter *ltre-files*
  '("ltms"      ;; LTMS
    "linter"    ;; Interface
    "ldata"     ;; Database
    "lrules"    ;; Rule system
    "unify"     ;; Unifier
    "funify"    ;; Open-coding of unification
    "laccept"   ;; shakedown tests for ltre
    "cwa"       ;; Closed-world assumption mechanism
    "dds"))     ;; Dependency-directed search facility

(defparameter *set-rule-file* "setrule")

;;; The file setrule.lisp should also be compiled for efficiency.
;;; It can only be compiled after an LTRE has been created.

(defun load-ltre (&key (action :compile-if-newer))
  (if (eq action :compile)
    (compile-ltre)
    (bps-load-files *ltre-path* *ltre-files* :action action)))

(defun compile-ltre ()
  (bps-load-files *ltre-path* *ltre-files* :action :compile)
  (unless (and (boundp '*ltre*)
               (not (null *ltre*)))
    (create-ltre "DUMMY"))
  (bps-load-file (make-bps-path "ltms") *set-rule-file* :action :compile))

