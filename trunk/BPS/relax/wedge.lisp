;; -*- Mode: Lisp; -*- 

;;; Wedge scene, from Winston Figure 3.6
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1988-1990, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(scene "Wedge")

(line L1)
(line L2)
(line L3)
(line L4)
(line L5)
(line L6)
(line L7)
(line L8)
(line L9)
(line L10)
(line L11)
(line L12)
(line L13)
(line L14)
(line L15)

(junction J1 Ell :LEFT L1 :RIGHT L2)
(junction J2 Arrow :LEFT L2 :RIGHT L3 :BOTTOM L9)
(junction J3 Ell :LEFT L3 :RIGHT L4)
(junction J4 Arrow :LEFT L4 :RIGHT L5 :BOTTOM L10)
(junction J5 Fork :LEFT L5 :RIGHT L6 :BOTTOM L12)
(junction J6 Arrow :LEFT L6 :RIGHT L7 :BOTTOM L14)
(junction J7 Ell :LEFT L7 :RIGHT L8)
(junction J8 Arrow :LEFT L8 :RIGHT L1 :BOTTOM L15)
(junction J9 Fork :LEFT L9 :RIGHT L11 :BOTTOM L10)
(junction J10 Arrow :LEFT L13 :RIGHT L11 :BOTTOM L12)
(junction J11 Fork :LEFT L15 :RIGHT L13 :BOTTOM L14)

