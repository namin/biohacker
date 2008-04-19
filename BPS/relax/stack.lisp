;; -*- Mode: Lisp; -*-

;;;; Stack -- another example
;; Last edited 1/29/93, by KDF

;; Copyright (c) 1988, 1989, 1991 Kenneth D. Forbus, Northwestern University,
;;   Johan de Kleer, Xerox Corporation.
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(Scene "Stack")

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
(line L16)
(line L17)

(Junction J1 Arrow :LEFT L9 :RIGHT L1 :BOTTOM L10)
(Junction J2 Fork :LEFT L1 :RIGHT L2 :BOTTOM L5)
(Junction J3 Arrow :LEFT L2 :RIGHT L3 :BOTTOM L16)
(Junction J4 Ell :LEFT L3 :RIGHT L4)
(Junction J5 Arrow :LEFT L4 :RIGHT L5 :BOTTOM L17)
(Junction J6 Ell :LEFT L5 :RIGHT L6)
(Junction J7 Tee :LEFT L6 :RIGHT L7 :BOTTOM L13)
(Junction J8 Arrow :LEFT L7 :RIGHT L8 :BOTTOM L12)
(Junction J9 Ell :LEFT L8 :RIGHT L9)
(Junction J10 Fork :LEFT L10 :RIGHT L11 :BOTTOM L12)
(Junction J11 Arrow :LEFT L14 :RIGHT L11 :BOTTOM L15)
(Junction J12 Fork :LEFT L16 :RIGHT L17 :BOTTOM L14)
