;; -*- Mode: Lisp; -*-

;;;; Cube example

;; Copyright (c) 1988, 1989, 1991 Kenneth D. Forbus, Northwestern University,
;;   Johan de Kleer, Xerox Corporation.
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(Scene "Cube")

(Line L1)
(Line L2)
(Line L3)
(Line L4)
(Line L5)
(Line L6)
(Line L7)
(Line L8)
(Line L9)

(Junction J1 Arrow :LEFT L6 :RIGHT L1 :BOTTOM L7)
(Junction J2 Ell :LEFT L1 :RIGHT L2)
(Junction J3 Arrow :LEFT L2 :RIGHT L3 :BOTTOM L8)
(Junction J4 Ell :LEFT L3 :RIGHT L4)
(Junction J5 Arrow :LEFT L4 :RIGHT L5 :BOTTOM L9)
(Junction J6 Ell :LEFT L6 :RIGHT L5)
(Junction J7 Fork :LEFT L7 :RIGHT L8 :BOTTOM L9)
