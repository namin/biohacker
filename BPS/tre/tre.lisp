;; -*- Mode: Lisp; -*-

;;; Tiny Rule Engine, Version 5
;; Last edited 2/6/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;; A very simple pattern-directed inference system.
;; This version is the simplest.

(defvar *tre-path*
  #+ILS "/u/bps/code/tre/"
  #+PARC "virgo:/virgo/dekleer/bps/code/jtms/"
  #+MCL "Macintosh HD:BPS:tre:tre.lisp")

(defvar *tre-files*
  '("tinter" ;; User interface
    "data"   ;; Assertions and database
    "rules"  ;; Storing and retrieving rules
    "unify"))  ;; Pattern matching & variables
