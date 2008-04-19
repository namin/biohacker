;; -*- Mode: Lisp; -*-

;;;; Operators for JSAINT
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defIntegration Integral-of-Constant
  (Integral ?t ?var) 
  :TEST (not (occurs-in? ?var ?t))
  :RESULT (* ?t ?var))

(defIntegration Integral-of-Self
  (Integral ?exp ?exp)
  :RESULT (/ (expt ?exp 2) 2))

(defIntegration Move-Constant-outside
  (Integral (* ?const ?nonconst) ?var)
  :TEST (and (not (occurs-in? ?var ?const))
	     (occurs-in? ?var ?nonconst))
  :SUBPROBLEMS ((?int (Integrate (Integral ?nonconst ?var))))
  :RESULT (* ?const ?int))

(defIntegration Integral-of-Sum
  (Integral (+ ?t1 ?t2) ?var)
  :SUBPROBLEMS ((?int1 (Integrate (Integral ?t1 ?var)))
		(?int2 (Integrate (Integral ?t2 ?var))))
  :RESULT (+ ?int1 ?int2))

(defIntegration Integral-of-Nary-sum
  (Integral (+ ?t1 ?t2 . ?trest) ?var)
  :SUBPROBLEMS ((?int1 (Integrate (Integral ?t1 ?var)))
		(?int2 (Integrate (Integral ?t2 ?var)))
		(?intr (Integrate (Integral (+ . ?trest) ?var))))
  :TEST (not (null ?trest))
  :RESULT (+ ?int1 ?int2 ?intr))

(defIntegration Integral-of-uminus
  (Integral (- ?term) ?var)
  :SUBPROBLEMS ((?int (Integrate (Integral ?term ?var))))
  :RESULT (- ?int))

(defIntegration Integral-of-minus
  (Integral (- ?t1 ?t2) ?var)
  :SUBPROBLEMS ((?int1 (Integrate (Integral ?t1 ?var)))
		(?int2 (Integrate (Integral ?t2 ?var))))
  :RESULT (- ?int1 ?int2))

(defIntegration Integral-of-SQR
  (Integral (sqr ?var) ?var)
  :RESULT (/ (expt ?var 3) 3))

(defIntegration Integral-of-polyterm
  (Integral (expt ?var ?n) ?var)
  :TEST (not (same-constant? ?n -1))
  :RESULT (/ (expt ?var (+ 1 ?n)) (+ 1 ?n)))

;;;; Some exponentials and trig functions

(defIntegration Simple-e-integral
  (Integral (expt %e ?var) ?var)
  :RESULT (expt %e ?var))

(defIntegration e-integral
  (Integral (expt %e (* ?a ?var)) ?var)
  :TEST (not (occurs-in? ?var ?a))
  :RESULT (/ (expt %e (* ?a ?var)) ?a))

(defIntegration non-e-power-integral
  (Integral (expt ?b (* ?a ?var)) ?var)
  :TEST (and (not (occurs-in? ?var ?a))
	     (not (occurs-in? ?var ?b)))
  :RESULT (/ (expt ?b (* ?a ?var)) (* ?a (log ?b %e))))

(defIntegration Log-Integral
  (Integral (log ?var %e) ?var)
  :RESULT (- (* ?var (log ?var %e)) ?var))

(defIntegration sin-integral
  (Integral (sin (* ?a ?var)) ?var)
  :TEST (not (occurs-in? ?var ?a))
  :RESULT (- (/ (cos (* ?a ?var)) ?a)))

(defIntegration cos-integral
  (Integral (cos (* ?a ?var)) ?var)
  :TEST (not (occurs-in? ?var ?a))
  :RESULT (/ (sin (* ?a ?var)) ?a))

(defIntegration sin-sqr-integral
  (Integral (sqr (sin ?var)) ?var)
  :RESULT (- (/ ?var 2) (/ (sin (* 2 ?var)) 4)))

(defIntegration cos-sqr-integral
  (Integral (sqr (cos ?var)) ?var)
  :RESULT (+ (/ ?var 2) (/ (sin (* 2 ?var)) 4)))

;;;; Some not-so-clever operators

(defIntegration SinToCosSqrSub
  (Integral ?exp ?var)
  :TEST (and (occurs-in? ?var ?exp)
	     (occurs-in? `(sin ,?var) ?exp))
  :SUBPROBLEMS
  ((?Int (Integrate (Integral
		     (:EVAL (subst `(sqrt (- 1 (expt (cos ,?var) 2)))
				   `(sin ,?var)
				   ?exp :TEST 'equal)) ?var))))
  :RESULT ?Int)

(defIntegration CosToSinSqrSub
  (Integral ?exp ?var)
  :TEST (and (occurs-in? ?var ?exp)
	     (occurs-in? `(cos ,?var) ?exp))
  :SUBPROBLEMS
  ((?Int (Integrate (Integral
		     (:EVAL (subst `(sqrt (- 1 (expt (sin ,?var) 2)))
				   `(cos ,?var)
				   ?exp :TEST 'equal)) ?var))))
  :RESULT ?Int)

(defIntegration SinSqrToTanCosSub
  (Integral ?exp ?var)
  :TEST (and (occurs-in? ?var ?exp)
	     (occurs-in? `(sin ,?var) ?exp))
  :SUBPROBLEMS ((?int (Integrate (Integral
				  (:EVAL (subst `(* (sqr (tan ,?var))
						    (sqr (cos ,?var)))
						`(sin ,?var)
						?exp :TEST 'equal))
				  ?var))))
  :RESULT ?Int)
