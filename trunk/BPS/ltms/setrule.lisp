;;; -*- Mode: Lisp; -*-

;;;; Rule for enforcing constraints on sets
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(rule ((:TRUE (set ?name) :VAR ?f1))
  (rule ((:INTERN (?name members ?construal1) :VAR ?f2))
    (rule ((:INTERN (?name has-member ?new) :VAR ?f3
		    :TEST (not (member ?new ?construal1
				       :TEST #'equal))))
	  (rassert! (:IMPLIES (:AND ?f1 ?f2) (:NOT ?f3))
		    :NOT-IN-SET))
    (rule ((:INTERN (?name MEMBERS ?construal2) :VAR ?f3
	     :TEST (and (form< ?f2 ?f3) ;; Avoid redundant nogoods
			(set-exclusive-or ?construal1
					  ?construal2
					  :TEST 'equal))))
	  (rassert! (:NOT (:AND ?f1 ?f2 ?f3))
		    :CONSTRUAL-UNIQUENESS))))

;;; It's important to avoid duplicate justifications, especially in
;;; large problems.  Do this by reifying justifications made by the
;;; CWA code in the LTRE database, using this rule to translate the
;;; statement into the appropriate clauses.  (These statements can
;;; safely be premises, because they are inviolate: only the
;;; statements which participate in them can be wrong.

(rule ((:INTERN (CWA-JUSTIFICATION ?ante ?conse) :VAR ?cwaj))
  (rassert! (:IMPLIES ?cwaj
		      (:IMPLIES ?ante ?conse))
	    :CWA-JUSTIFICATION))
