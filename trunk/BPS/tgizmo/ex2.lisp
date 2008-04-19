;;; -*- Syntax: Common-lisp; Mode: LISP; -*-

;;;; Water on stove example
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(rassert! (substance water))
(rassert! (phase liquid))
(rassert! (phase gas))

(rassert! (Container Can))

(rassert! (Temperature-Source Stove))

(rassert! (Heat-Path Burner))

(rassert! (Heat-Connection Burner Stove (c-s water gas can)))
(rassert! (Heat-Connection Burner Stove (c-s water liquid can)))

