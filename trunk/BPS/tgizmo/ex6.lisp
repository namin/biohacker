;;; -*- Syntax: Common-lisp; Mode: LISP; -*-

;;;; Four blobs example
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package  :COMMON-LISP-USER)

(assertq (physob F))
(assertq (physob G))
(assertq (physob H))
(assertq (physob I))

(assertq (heat-path P1))
(assertq (heat-path P2))
(assertq (heat-path P3))

(assertq (heat-connection P1 F G))
(assertq (heat-connection P1 G F))
(assertq (heat-connection P2 G H))
(assertq (heat-connection P2 H G))
(assertq (heat-connection P3 G I))
(assertq (heat-connection P3 I G))


