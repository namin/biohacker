;;; -*- Mode: LISP; Syntax: Common-lisp; Package: USER -*-

;;; Copyright (c) 1986-1992 Kenneth D. Forbus, Johan de Kleer and 
;;; Xerox Corporation.  All Rights Reserved.
 
;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(constraint adder-component ((a1 cell) (a2 cell) (sum cell)
			     (ok assumption))
	    (formulae (sum (a1 a2 ok) (+ a1 a2))
		      (a1 (sum a2 ok) (- sum a2))
		      (a2 (sum a1 ok) (- sum a1))))

(constraint multiplier-component ((m1 cell) (m2 cell)
				  (product cell) (ok assumption))
 (formulae (product (m1) (if (nearly-equal? 0 m1) 0.0 :dismiss))
	   (product (m2) (if (nearly-equal? 0 m2) 0.0 :dismiss))
	   (product (m1 m2 ok) (cond ((or (nearly-equal? 0 m1)
					  (nearly-equal? 0 m2))
				      :dismiss)
				     (t (* m1 m2))))
	   (m1 (product m2 ok) (if (nearly-equal? 0 m2) :dismiss
				   (/ product m2)))
	   (m2 (product m1 ok) (if (nearly-equal? 0 m1) :dismiss
				   (/ product m1)))))

(constraint poly ((a cell 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
		  (b cell 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
		  (c cell 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
		  (d cell 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
		  (e cell 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)	
		  (x cell 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
		  (y cell 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
		  (z cell 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
		  (f cell 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
		  (g cell 0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)
		  (m1 multiplier-component a c x)
		  (m2 multiplier-component b d y)
		  (m3 multiplier-component c e z)
		  (a1 adder-component x y f)
		  (a2 adder-component y z g)))

(assume-constraint xor-component ((in1 cell 0 1) (in2 cell 0 1)
				  (out cell 0 1))
	    (formulae (out (in1 in2) (if (= in1 in2) 0 1))
		      (in1 (out in2) (if (= in2 out) 0 1))
 		      (in2 (out in1) (if (= in1 out) 0 1))))

(assume-constraint or-component ((in1 cell 0 1) (in2 cell 0 1)
				 (out cell 0 1))
	    (formulae (out (in1 in2)
			   (if (and (= in1 0) (= in2 0)) 0 :dismiss))
		      (out (in1) (if (= in1 1) 1 :dismiss))
		      (out (in2) (if (= in2 1) 1 :dismiss))
		      (in1 (out in2)
			   (if (and (= out 1) (= in2 0)) 1 :dismiss))
		      (in2 (out in1)
			   (if (and (= out 1) (= in1 0)) 1 :dismiss))
		      (in1 (out) (if (= out 0) 0 :dismiss))
		      (in2 (out) (if (= out 0) 0 :dismiss))))

(assume-constraint and-component ((in1 cell 0 1) (in2 cell 0 1)
				  (out cell 0 1))
	    (formulae (out (in1 in2)
			   (if (and (= in1 1) (= in2 1)) 1 :dismiss))
		      (out (in1) (if (= in1 0) 0 :dismiss))
		      (out (in2) (if (= in2 0) 0 :dismiss))
		      (in1 (out) (if (= out 1) 1 :dismiss))
		      (in2 (out) (if (= out 1) 1 :dismiss))
		      (in1 (out in2) (if (and (= out 0) (= in2 1)) 0 :dismiss))
		      (in2 (out in1) (if (and (= out 0) (= in1 1)) 0 :dismiss))))

(constraint full-adder ((co cell) (ci cell) (a cell) (b cell) (q cell)
			(x cell) (y cell) (z cell)
			(x1 xor-component a b z)
			(a1 and-component a b y)
			(x2 xor-component ci z q)
			(a2 and-component ci z x)
			(o1 or-component x y co)))

(constraint 2-bit-adder ((ripple cell)
			 (bit0 full-adder ripple)
			 (bit1 full-adder () ripple)))