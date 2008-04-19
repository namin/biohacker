; -*- Mode: Lisp; -*-

;;;; N-Queens rules, FTRE version
;;;;  Modified: forbus on Tue Apr 2 10:24:59 1996

;;; Copyright (c) 1992-1996 Kenneth D. Forbus, Northwestern University,
;;; Johan de Kleer and Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(rule ((queen ?column1 ?row1)
       (queen ?column2 ?row2)
       :TEST (not (or (= ?column1 ?column2)
		      (queens-okay? ?column1 ?row1
				    ?column2 ?row2))))
      (rassert! Contradiction))

