;; -*- Mode: Lisp; -*-

;;;; Polybox example
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
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
	   (product (m1 m2) (if (or (nearly-zero? m1)
				    (nearly-zero? m2))
				   :DISMISS)
				  (* m1 m2))
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

(constraint Polybox-Example ((a cell)(b cell)(c cell)
		             (d cell)(e cell)(f cell)
		             (g cell) (x cell)(y cell)(z cell)
		             (add-1 adder)(add-2 adder)
		             (mult-1 multiplier)
		             (mult-2 multiplier)
		             (mult-3 multiplier))
  (== (>> a) (>> m1 mult-1))
  (== (>> b) (>> m1 mult-2))
  (== (>> c) (>> m2 mult-1))
  (== (>> c) (>> m1 mult-3))
  (== (>> d) (>> m2 mult-2))
  (== (>> e) (>> m2 mult-3))
  (== (>> f) (>> sum add-1))
  (== (>> g) (>> sum add-2))
  (== (>> x) (>> product mult-1)) 
  (== (>> x) (>> a1 add-1)) 
  (== (>> y) (>> product mult-2))
  (== (>> y) (>> a2 add-1))
  (== (>> y) (>> a1 add-2))
  (== (>> z) (>> product mult-3))
  (== (>> z) (>> a2 add-2)))
