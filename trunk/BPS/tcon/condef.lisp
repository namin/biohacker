;;; -*- Mode: Lisp; Syntax: Common-lisp; -*-

;;;; Sample constraints for TCON interpreter.
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1988-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(constraint adder ((a1 cell) (a2 cell) (sum cell))
	    (formulae (sum (a1 a2) (+ a1 a2))
		      (a1 (sum a2) (- sum a2))
		      (a2 (sum a1) (- sum a1))))

(constraint multiplier ((m1 cell) (m2 cell) (product cell))
 (formulae (product (m1) (if (nearly-zero? m1) 0.0 :DISMISS))
	   (product (m2) (if (nearly-zero? m2) 0.0 :DISMISS))
	   (product (m1 m2) (cond ((or (nearly-zero? m1)
				       (nearly-zero? m2))
				   :DISMISS)
				  (t (* m1 m2))))
	   (m1 (product m2) (if (nearly-zero? m2)
				(if (nearly-zero? product)
				    :DISMISS
				  :LOSE)
			      (/ product m2)))
	   (m2 (product m1) (if (nearly-zero? m1)
				(if (nearly-zero? product)
				    :DISMISS
				  :LOSE)
			      (/ product m1)))))

(constraint sign ((in cell) (out cell))
	    (formulae (out (in) (if (nearly-zero? in) 0.0
				    (signum in)))
		      (in (out) (if (= 0.0 out) 0.0 :DISMISS))))

(constraint magnitude ((in cell) (out cell))
	    (formulae (out (in) (if (nearly-zero? in) 0.0
				    (abs in)))
		      (in (out) (if (= 0.0 out)
				    0.0 :DISMISS))))

(constraint sign-magnitude ((number cell) (sign cell)
			    (magnitude cell)
			    (s sign) (m magnitude)
			    (prod multiplier))
	    (== (>> sign) (>> out s))
	    (== (>> magnitude) (>> out m))
	    (== (>> number) (>> in s))
	    (== (>> number) (>> in m))
	    ;Use multiplier constraint
	    (== (>> number) (>> product prod))
	    (== (>> sign) (>> m1 prod))
	    (== (>> magnitude) (>> m2 prod)))

;;;; Temperature conversion (a classic)

(constraint F-to-C ((degF cell) (degC cell)
		    (sum adder)(m multiplier))
	    (== (>> degF) (>> sum sum))
	    (Constant (>> a1 sum) 32)
	    (== (>> a2 sum) (>> product m))
	    (== (>> degC) (>> m1 m))
	    (Constant (>> m2 m) (float (/ 9 5))))

;;;; Primitive boolean constraints.

(constraint inverter ((in cell) (out cell))
      (formulae (in (out) (not out))
		(out (in) (not in))))

(constraint implication ((antecedent cell) (consequent cell))
 (formulae (consequent (antecedent)
		       (cond (antecedent 'T) (t  :DISMISS)))
	   (antecedent (consequent)
		       (cond (consequent :DISMISS) (t 'NIL)))))

(constraint or-gate ((in1 cell) (in2 cell) (out cell))
      (formulae (out (in1 in2) (or in1 in2))
		(out (in1) (cond (in1 t) (t :DISMISS)))
		(out (in2) (cond (in2 t) (t :DISMISS)))
		(in1 (out in2) (cond ((not in2) out)
				     (t :DISMISS)))
		(in2 (out in1) (cond ((not in1) out)
				     (t :DISMISS)))))

(constraint and-gate ((in1 cell) (in2 cell) (out cell))
	    (formulae (out (in1 in2) (and in1 in2))
		      (out (in1) (cond (in1 :DISMISS)
				       (t nil)))
		      (out (in2) (cond (in2 :DISMISS)
				       (t nil)))
		      (in1 (out in2) (cond (in2 out)
					   (T :DISMISS)))
		      (in2 (out in1) (cond (in1 out)
					   (t :DISMISS)))))

(constraint relay ((in1 cell) (in2 cell) (out cell)
		   (control cell))
  (formulae (in1 (out control) (cond (control out)
				     (t :DISMISS)))
	    (in2 (out control) (cond (control :DISMISS)
				     (t out)))
	    (out (in1 control) (cond (control in1)
				     (t :DISMISS)))
	    (out (in2 control) (cond (control :DISMISS)
				    (t in2)))))


