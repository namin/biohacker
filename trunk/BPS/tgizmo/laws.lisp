;; -*- Mode: Lisp; -*-

;;;; Laws of QP theory for TGizmo
;;; Last Edited: 1/29/93, by KDF

;;; Copyright (c) 1991, Kenneth D. Forbus, Northwestern University,
;;;  and Johan de Kleer, the Xerox Corporation.
;;; All Rights Reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;; This file implements a subset of QP theory, using
;;; the LTMS to directly encode axiomatic statements.

(in-package :COMMON-LISP-USER)

(rule ((:INTERN (Quantity (?qtype ?individual)) :VAR ?qdecl))
      ;; Greatly restricted
      (rassert! (:IMPLIES ?qdecl (Exists ?individual))
                :QUANTITY-EXISTENCE)
      (rassert! (set (DIs (?qtype ?individual))) :DIS-DEF)
      (rassert! (set (IIs (?qtype ?individual))) :IIS-DEF)
      (push (list ?qtype ?individual) (tgizmo-quantities *tgizmo*)))

;;; In the next five rules, 
;;;    ?n1, ?n2 = (<A or D> (?qtype ?individual)) | <constant>

(rule ((:INTERN (> ?n1 ?n2) :VAR ?gt))
      (install-comparison-constraints-if-needed ?n1 ?n2)
      (rassert! (:IFF ?gt (:AND (?n2 <= ?n1)
				(:NOT (?n1 <= ?n2))))
                :>-DEF))

(rule ((:INTERN (< ?n1 ?n2) :VAR ?lt))
      (install-comparison-constraints-if-needed ?n1 ?n2)
      (rassert! (:IFF ?lt (:AND (?n1 <= ?n2)
				(:NOT (?n2 <= ?n1))))
		:<-DEF))

(rule ((:INTERN (= ?n1 ?n2) :VAR ?eq))
      (install-comparison-constraints-if-needed ?n1 ?n2)
      (rassert! (:IFF ?eq (:AND (?n1 <= ?n2)
				(?n2 <= ?n1)))
		:=-DEF))

(rule ((:INTERN (>= ?n1 ?n2) :VAR ?gte))
      (install-comparison-constraints-if-needed ?n1 ?n2)
      (rassert! (:IFF ?gte (?n2 <= ?n1)) :>=-DEF))

(rule ((:INTERN (<= ?n1 ?n2) :VAR ?lte))
      (install-comparison-constraints-if-needed ?n1 ?n2)
      (rassert! (:IFF ?lte (?n1 <= ?n2)) :<=-DEF))

;;;; Accumulating process and view structures

(rule ((:TRUE (Active ?p) :VAR ?aform))
      ;; If active, it's in there.
      (rule ((:TRUE (Process-instance ?p) :VAR ?pform))
	    (rassert! (:IMPLIES (:AND ?pform ?aform)
				(PS has-member ?p))
		      :PS-MEMBER))
      (rule ((:TRUE (View-instance ?p) :VAR ?pform))
	    (rassert! (:IMPLIES (:AND ?pform ?aform)
				(VS has-member ?p))
		      :VS-MEMBER)))

(rule ((:FALSE (Active ?p) :VAR ?aform))
      ;; If inactive, it's known to not be in there.
      (rule ((:TRUE (Process-instance ?p) :VAR ?pform))
	    (rassert! (:IMPLIES (:AND ?pform (:NOT ?aform))
				(:NOT (PS has-member ?p)))
		      :NOT-PS-MEMBER))
      (rule ((:TRUE (View-instance ?p) :VAR ?pform))
	    (rassert! (:IMPLIES (:AND ?pform (:NOT ?aform))
				(:NOT (VS has-member ?p)))
		      :NOT-VS-MEMBER)))

;;;; Accumulating influences

(rule ((:INTERN (Quantity ?q) :VAR ?qform))
      (rassert! (:IFF (:OR (:NOT ?qform) ((DIs ?q) members nil))
		      (:NOT (Directly-Influenced ?q)))
		:DIS-DEFINITION)
      (rassert! (:IFF (:OR (:NOT ?qform) ((IIs ?q) members nil))
		      (:NOT (Indirectly-Influenced ?q)))
		:IIS-DEFINITION)
      (rassert! (:NOT (:AND ?qform
			    (Directly-Influenced ?q)
			    (Indirectly-Influenced ?q)))
		:QP-CONSISTENCY-LAW)
      (rassert! (:IMPLIES (:AND ?qform
				(:NOT (Directly-Influenced ?q))
				(:NOT (Indirectly-Influenced ?q)))
			  (= (D ?q) ZERO))
		:UNINFLUENCED-DEFINITION))

(rule ((:TRUE (I+ ?influenced ?influencer ?source) :VAR ?Is))
      (rassert! (:IMPLIES ?Is (Directly-Influenced ?influenced))
		:DIS-DEFINITION)
      (rassert! (:IFF ?Is ((DIs ?influenced) has-member ?Is))
		:DIS-DEFINITION))

(rule ((:TRUE (I- ?influenced ?influencer ?source) :VAR ?Is))
      (rassert! (:IMPLIES ?Is (Directly-Influenced ?influenced))
		:DIS-DEFINITION)
      (rassert! (:IFF ?Is ((DIs ?influenced) has-member ?Is))
		:DIS-DEFINITION))

(rule ((:TRUE (Qprop ?influenced ?influencer ?source) :VAR ?Is))
      (rassert! (:IMPLIES ?Is (Indirectly-Influenced ?influenced))
		:IIS-DEFINITION)
      (rassert! (:IFF ?Is ((IIs ?influenced) has-member ?Is))
		:IIS-DEFINITION))

(rule ((:TRUE (Qprop- ?influenced ?influencer ?source) :VAR ?Is))
      (rassert! (:IMPLIES ?Is (Indirectly-Influenced ?influenced))
		:IIS-DEFINITION)
      (rassert! (:IFF ?Is ((IIs ?influenced) has-member ?Is))
		:IIS-DEFINITION))
