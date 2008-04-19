;;; -*- Syntax: Common-lisp; Mode: Lisp; -*-

;;;; double-heat flow example
;; Last edited 1/29/93

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(assert! (physob rock))
(assert! (Exists rock))

(assert! (Temperature-Source Stove))

(assert! (Temperature-Source ATM))

(assert! (Heat-Path Burner))

(assert! (Heat-Path (surface rock)))

(assert! (Heat-Connection Burner Stove rock))
(assert! (Heat-Connection Burner rock stove))

(assert! (Heat-Connection (surface rock) ATM rock))
(assert! (Heat-Connection (surface rock) rock ATM))

(assert! (Less-Than (A (temperature ATM)) (A (temperature Stove))))
