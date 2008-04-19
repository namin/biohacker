;; -*- MODE: Lisp; -*-

;;; ATRE: Tiny Rule Engine, with ATMS interface
;; Last edited: 1/29/93, KDF

;; Copyright (c) 1992, Kenneth D. Forbus, Northwestern
;; University, and Johan de Kleer, the Xerox Corporation
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defvar *atre-path*
  #+ILS "/u/bps/code/atms/"
  #+PARC "virgo:/virgo/dekleer/bps/code/atms/"
  #+MCL "Macintosh HD:BPS:atms:")

(setq *atre-files*
  '("atms"      ;; ATMS
    "ainter"    ;; Interface
    "adata"     ;; Database
    "arules"    ;; Rule system
    "unify"     ;; Variables and pattern matching
    "funify"    ;; Open-coding of unification
    "atret"))   ;; Test procedures

(setq *planner-files*
      '("aplanr"  ;; Utilities
	"plan-a"  ;; Antecedent planner
	"plan-e"  ;; Envisioner
	"bcode"   ;; Blocks World support
	"blocks")) ;; Rules for Blocks World

(defun compile-planner () ;; Assumes ATRE is compiled and loaded.
  (compile-load-files '("aplanr" "plan-a" "plan-e" "bcode")
		      *atre-path*)
  (unless (and (boundp '*plnpr*)
	       (not (null *plnpr*)))
	  (create-planning-problem "DUMMY" nil))
  (compile-load-files '("blocks") *atre-path*))








