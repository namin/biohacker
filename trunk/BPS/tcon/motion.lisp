;;; -*- Mode: Lisp; Syntax: Common-lisp; -*-

;;;; Constraints for motion problems
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1991-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;; Start with basic math



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

(constraint sqr ((in cell)(out cell))
 (formulae (out (in) (* in in))
	   (in (out) (if (< out 0.0) :LOSE
		       (sqrt out)))))

(constraint number ((value cell)
		    (magnitude cell)
		    (sign cell))
 (formulae (magnitude (value) (abs value))
	   (sign (value) (signum value))
	   (value (magnitude sign) (* sign magnitude))))

(constraint cos ((in cell)(out cell))
 (formulae (out (in) (cos in))
	   (in (out) (acos out))))

(constraint sin ((in cell)(out cell))
 (formulae (out (in) (sin in))
	   (in (out) (asin out))))

(constraint atan ((out cell)(x cell)(y cell))
 (formulae (out (x y)
		(if (and (nearly-zero? x)
			 (nearly-zero? y))
			 :DISMISS
			 (atan y x)))))

;;;; Vectors

(constraint 2D-vector ((x cell)(y cell)
		       (signx cell) (signy cell)
		       (sgnx sign) (sgny sign)
		       (r cell)(theta cell)
		       (quadrant cell))
 (== (>> x) (>> in sgnx))
 (== (>> signx) (>> out sgnx))
 (== (>> y) (>> in sgny))
 (== (>> signy) (>> out sgny))
 (formulae (quadrant (signx signy)
		     (quadrant-from-signs signx signy))
	   (signx (quadrant)
		  (multiple-value-bind (x-sign y-sign)
		    (signs-from-quadrant quadrant)
		    x-sign))
	   (signy (quadrant)
		  (multiple-value-bind (x-sign y-sign)
		    (signs-from-quadrant quadrant)
		    y-sign))
	   (theta (quadrant)
		  (angle-from-quadrant quadrant))
	   (x (r theta) (if (nearly-zero? r) :DISMISS
			  (* r (cos theta))))
	   (y (r theta) (if (nearly-zero? r) :DISMISS
			  (* r (sin theta))))
	   (theta (r x quadrant)
		  (if (or (nearly-zero? r)
			  (not (simple-quadrant? quadrant))) :DISMISS
		    (acos-corrected (/ x r) quadrant)))
	   (theta (r y quadrant)
		  (if (or (nearly-zero? r)
			  (not (simple-quadrant? quadrant))) :DISMISS
			  (asin-corrected (/ y r) quadrant)))
	   (x (r) (if (nearly-zero? r) 0.0 :DISMISS))
	   (y (r) (if (nearly-zero? r) 0.0 :DISMISS))
	   (r (x y) (sqrt (+ (* x x) (* y y))))
	   (theta (x y) (if (and (nearly-zero? x)
				 (nearly-zero? y))
			    0.0 ;; By convention
			    (atan-corrected y x)))))

;;;; Trig support for constraints
;;
;; Keeps all angles within [0,2pi) for consistency

(defun simple-quadrant? (quad) (integerp quad))

(defun quadrant-from-signs (signx signy)
  (if (< signx 0)
      (if (< signy 0) 3
	(if (> signy 0) 2 '2-3))
    (if (> signx 0)
	(if (< signy 0) 4
	  (if (> signy 0) 1 '1-4))
      (if (< signy 0) '3-4
	(if (> signy 0) '1-2 '0-0)))))

(defun angle-from-quadrant (quadrant)
  (if (simple-quadrant? quadrant) :DISMISS
    (case quadrant
	  (0-0 0.0)(1-2 (/ pi 2.0))
	  (2-3 pi)(3-4 (/ (* 3 pi) 2.0))
	  (1-4 0.0)(t :DISMISS))))

(defun signs-from-quadrant (quadrant)
  (case quadrant
	(0-0 (values 0.0 0.0))
	(1 (values 1.0 1.0))
	(1-2 (values 0.0 1.0))
	(2 (values -1.0 1.0))
	(2-3 (values -1.0 0.0))
	(3 (values -1.0 -1.0))
	(3-4 (values 0.0 -1.0))
	(4 (values 1.0 -1.0))
	(1-4 (values 1.0 0.0))
	(else :DISMISS)))

(defun atan-corrected (arg1 arg2)
  (let ((theta (atan arg1 arg2)))
    (if (< theta 0.0) (+ theta (* pi 2))
      theta)))

(defun acos-corrected (result quadrant)
  (let ((theta (acos result)))
    (if (= quadrant 3) (- (* pi 2) theta)
      (if (= quadrant 4) (- (* pi 2) theta)
	theta))))

(defun asin-corrected (result quadrant)
  (let ((theta (asin result)))
    (if (= quadrant 2) (- pi theta)
      (if (= quadrant 3) (- pi theta)
	(if (= quadrant 4) (+ theta (* pi 2))
	  theta)))))

;;;; Uniform motion example
;; Equation is Xend = Xstart + dT*Vx

(constraint 1D-uniform-motion ((xstart cell) (xend cell)
			       (velocity cell)(deltat cell)
			       (adr adder)(mul multiplier))
    (== (>> xstart) (>> a1 adr))
    (== (>> xend) (>> sum adr))
    (== (>> velocity) (>> m1 mul))
    (== (>> deltat) (>> m2 mul))
    (== (>> product mul) (>> a2 adr)))

(constraint 1D-uniform-accel ((accel cell)(deltat cell)
			      (deltax cell)(sqr sqr)
			      (mul1 multiplier)
			      (mul2 multiplier))
 (== (>> deltax) (>> product mul1))
 (== (>> accel) (>> m1 mul1))
 (== (>> deltat) (>> in sqr))
 (== (>> m2 mul1) (>> product mul2))
 (== (>> m1 mul2) (>> out sqr))
 (constant (>> m2 mul2) 0.5))

(constraint 1D-motion ((xstart cell)(xend cell)
		       (vstart cell)(vend cell)
		       (accel cell)(deltat cell)
		       (basic 1D-uniform-motion)
		       (fix 1D-uniform-accel)
		       (adr1 adder)(adr2 adder)
		       (mul multiplier))
 (== (>> deltat) (>> deltat basic))	    
 (== (>> deltat) (>> deltat fix))	    
 (== (>> deltat) (>> m1 mul))
 (== (>> xstart) (>> xstart basic))
 (== (>> xend) (>> sum adr1))
 (== (>> vstart) (>> velocity basic))
 (== (>> vstart) (>> a1 adr2))
 (== (>> vend) (>> sum adr2))
 (== (>> accel) (>> accel fix))
 (== (>> accel) (>> m2 mul))
 (== (>> xend basic) (>> a1 adr1))
 (== (>> deltax fix) (>> a2 adr1))
 (== (>> product mul) (>> a2 adr2))
 (formulae (deltat (xstart xend vstart accel)
             (if (nearly-zero? accel) :DISMISS
		   (let* ((det (sqrt (- (* vstart vstart)
				       (* 2.0 accel
					  (- xstart xend)))))
			  (ans1 (/ (- (- vstart) det) accel))
			  (ans2 (/ (+ (- vstart) det) accel)))
		     (if (< ans1 0.0)
			 (if (< ans2 0.0) :DISMISS ans2)
		       (if (< ans2 0.0) ans1
			 :DISMISS)))))))

;;;; 2D motion

(constraint 2D-Motion ((start 2D-vector)(vstart 2D-vector)
		       (end 2D-vector)(vend 2D-vector)
		       (xaccel cell)(yaccel cell)
		       (deltat cell)
		       (xmotion 1D-motion)
		       (ymotion 1D-motion))
 (== (>> deltat) (>> deltat xmotion))
 (== (>> deltat) (>> deltat ymotion))
 (== (>> xaccel) (>> accel xmotion))
 (== (>> yaccel) (>> accel ymotion))
 (== (>> x start) (>> xstart xmotion))
 (== (>> y start) (>> xstart ymotion))
 (== (>> x end) (>> xend xmotion))
 (== (>> y end) (>> xend ymotion))
 (== (>> x vstart) (>> vstart xmotion))
 (== (>> y vstart) (>> vstart ymotion))
 (== (>> x vend) (>> vend xmotion))
 (== (>> y vend) (>> vend ymotion)))

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
