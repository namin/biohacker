;; -*- Mode: Lisp; -*-

;;;; File specifications for CPS, the "Classical Problem Solver"
;;; Last edited: 2/6/93, KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(setq *cps-path*
      #+ILS "/u/bps/code/cps/"
      #+PARC "virgo:/virgo/dekleer/bps/code/cps/"
      #+MCL "Macintosh HD:BPS:cps:")

(defvar *cps-files* '("search" "variants"))
(defvar *algebra-files* '("match" "algebra" "simplify"))
(defvar *subway-files* '("subways" "boston"))

(defun compile-cps ()
  (compile-load-files *cps-files* *cps-path*)
  (compile-load-files *algebra-files* *cps-path*)
  (compile-load-files *subway-files* *cps-path*))

