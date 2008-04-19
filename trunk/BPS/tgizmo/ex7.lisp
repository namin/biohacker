;;; -*- Mode: LISP; Syntax: Common-lisp -*-

;;;; Four containers example
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package  :COMMON-LISP-USER)

(assertq (state liquid))
(assertq (substance water))

(assertq (container F))
(assertq (container G))
(assertq (container H))
(assertq (container I))

(assertq (fluid-path P1))
(assertq (fluid-path P2))
(assertq (fluid-path P3))

(assertq (fluid-connection P1 F G))
(assertq (fluid-connection P1 G F))
(assertq (fluid-connection P2 G H))
(assertq (fluid-connection P2 H G))
(assertq (fluid-connection P3 G I))
(assertq (fluid-connection P3 I G))

(assertq (Equal-to (A (bottom-height F)) (A (Max-Height P1))))
(assertq (Equal-to (A (bottom-height H)) (A (Max-Height P2))))
(assertq (Equal-to (A (bottom-height I)) (A (Max-Height P3))))

(assertq (Equal-to (A (bottom-height G)) (A (max-height P1))))
(assertq (Equal-to (A (bottom-height G)) (A (max-height P2))))
(assertq (Equal-to (A (bottom-height G)) (A (max-height P3))))
