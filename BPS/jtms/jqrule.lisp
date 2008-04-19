;-*- Mode:  Lisp; Syntax: Common-lisp -*-

;;;; N-Queens rules, JTRE version
;;; Last edited 1/29/93, by KDF.

;;; Copyright (c) 1986 --- 1992 Kenneth D. Forbus, Northwestern University,
;;; Johan de Kleer and Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(contradiction 'Queens-capture *jtre*)

(rule ((:IN (Queen ?column1 ?row1) :VAR ?Q1)
       (:IN (Queen ?column2 ?row2) :VAR ?Q2
         :TEST (not (or (= ?column1 ?column2)
			(queens-okay? ?column1 ?row1
				      ?column2 ?row2)))))
      (rassert! Queens-capture (Death ?Q1 ?Q2)))

