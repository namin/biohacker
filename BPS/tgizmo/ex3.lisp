;;; -*- Syntax: Common-lisp; Mode: Lisp; -*-

;;; Three containers example
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1991-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(rassert! (phase liquid))
(rassert! (substance water))

(rassert! (container F))
(rassert! (container G))
(rassert! (container H))

(rassert! (fluid-path P1))
(rassert! (fluid-path P2))

(rassert! (Aligned P1))
(rassert! (Aligned P2))

(rassert! (fluid-connection P1 F G))
(rassert! (fluid-connection P1 G F))
(rassert! (fluid-connection P2 G H))
(rassert! (fluid-connection P2 H G))


