;; -*- Mode: Lisp; -*- 

;;;; Two containers scenario
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(rassert! (phase liquid))
(rassert! (substance water))

;; Declare indivduals and their types
(rassert! (container F))
(rassert! (container G))
(rassert! (fluid-path P1))

;; Specify their connectivity
(rassert! (fluid-connection P1 F G))
(rassert! (fluid-connection P1 G F))



