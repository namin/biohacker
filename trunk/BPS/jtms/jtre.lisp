;; -*- Mode: LISP; -*-

;;;; JTRE -- a version of TRE which uses the JTMS
;;; Last Edited, 1/29/93, by KDF

;;; Copyright (c) 1992, Kenneth D. Forbus, Northwestern University,
;;; Johan de Kleer and Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defvar *jtre-path*
  #+ILS "/u/bps/code/jtms/"
  #+PARC "virgo:/virgo/dekleer/bps/code/jtms/"
  #+MCL "Macintosh HD:BPS:jtms:")

(defvar *jtre-files*
  '("jtms"      ;; JTMS
    "jinter"    ;; Interface
    "jdata"     ;; Database
    "jrules"    ;; Rule system
    "unify"     ;; Unifier
    "funify"))  ;; Open-coding unification

(setq *jqueens-files*
      '("jqueens"    ;; JTRE version of N-queens puzzle
        "jqrule"))   ;; Contradiction detection rule

(setq *jsaint-files*
      '("jsaint"   ;; JSAINT main program
	"match"    ;; math-oriented pattern matcher
	"simplify" ;; Algebraic simplifier 
        ;; These two files can only be compiled after a JSAINT
        ;; has been created:
        "jsrules"  ;; Bookkeeping rules
        "jsops"    ;; Sample integration library
        ))

(defun compile-jqueens ()
  (unless (and (boundp '*jtre*)
               (not (null *jtre*)))
    (in-jtre (create-jtre "Dummy")))
  (compile-load-files *jqueens-files* *jtre-path*))

(defun compile-jsaint ()
  (compile-load-files '("jsaint" "match" "simplify") *jtre-path*)
  (unless (and (boundp '*jsaint*)
               (not (null *jsaint*)))
    (create-jsaint "Dummy" nil))
  (compile-load-files '("jsrules" "jsops") *jtre-path*))
