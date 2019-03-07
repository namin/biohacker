;; -*- Mode: Lisp; -*-

;;;; A simple domain theory for TGizmo
;;; Last Edited: 1/29/93, by KDF

;;; Copyright (c) 1991-1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defentity (Container ?can)
  (quantity (pressure ?can))) ;; at bottom

(defentity (fluid-path ?path))
(defentity (heat-path ?path))

(defentity (Physob ?phob)
  (quantity (heat ?phob))
  (quantity (temperature ?phob))
  (> (A (heat ?phob)) ZERO)
  (> (A (temperature ?phob)) ZERO)
  (qprop (temperature ?phob) (heat ?phob)))

(defentity (Temperature-Source ?phob)
  (quantity (heat ?phob))
  (quantity (temperature ?phob))
  (> (A (heat ?phob)) ZERO)
  (> (A (temperature ?phob)) ZERO))

(defrule Contained-Stuff-Existence
  ((Container ?can)(Phase ?st)(Substance ?sub))
  ;; Assume that every kind of substance can exist in
  ;; in every phase inside every container.
  (quantity ((amount-of ?sub ?st) ?can))
  (>= (A ((amount-of ?sub ?st) ?can)) ZERO))

(defview (Contained-Stuff (C-S ?sub ?st ?can))
  :INDIVIDUALS ((?can (container ?can)
		      (substance ?sub)
		      (phase ?st)))
  :QUANTITY-CONDITIONS ((> (A ((amount-of ?sub ?st) ?can)) ZERO))
  :RELATIONS ((Only-During (Exists (C-S ?sub ?st ?can)))
	      (Contained-stuff (C-S ?sub ?st ?can))
	      (quantity (TBoil (C-S ?sub ?st ?can)))
	      (> (A (TBoil (C-S ?sub ?st ?can))) ZERO)))

(defentity (Contained-Stuff (C-S ?sub liquid ?can))
  (Contained-Liquid (C-S ?sub liquid ?can)))

(defentity (Contained-Liquid (C-S ?sub liquid ?can))
  (physob (C-S ?sub liquid ?can))
  (quantity (level (C-S ?sub liquid ?can)))
  (qprop (level (C-S ?sub liquid ?can))
	 ((Amount-of ?sub liquid) ?can))
  (qprop (pressure ?can) (level (C-S ?sub liquid ?can))))

(defentity (Contained-Stuff (C-S ?sub gas ?can))
  (Contained-gas (C-S ?sub gas ?can)))

(defentity (Contained-Gas (C-S ?sub gas ?can))
  (physob (C-S ?sub gas ?can))
  (qprop (pressure ?can)
	 (temperature (C-S ?sub gas ?can)))
  (qprop (pressure ?can)
	 ((amount-of ?sub gas) ?can)))

;;;; Flow processes

(defprocess (heat-flow ?src ?path ?dst)
  :INDIVIDUALS ((?src (Quantity (heat ?src)))
		(?path (Heat-Connection ?path ?src ?dst))
		(?dst (Quantity (heat ?dst))))
  :PRECONDITIONS ((Heat-Aligned ?path))
  :QUANTITY-CONDITIONS ((> (A (temperature ?src))
			   (A (temperature ?dst))))
  :RELATIONS ((Quantity (flow-rate ?self))
	      (> (A (flow-rate ?self)) zero)
	      (Qprop (flow-rate ?self) (temperature ?src))
	      (Qprop- (flow-rate ?self) (temperature ?dst)))
  :INFLUENCES ((I- (heat ?src) (flow-rate ?self))
	       (I+ (heat ?dst) (flow-rate ?self))))

(defprocess (fluid-flow (C-S ?sub ?st ?src) ?path ?dst)
  :INDIVIDUALS ((?src (container ?src)
		      (substance ?sub) (phase ?st))
		(?path (fluid-Connection ?path ?src ?dst))
		(?dst (container ?dst)))
  :PRECONDITIONS ((Aligned ?path))
  :QUANTITY-CONDITIONS ((> (A ((amount-of ?sub ?st) ?src)) ZERO)
;;;			(> (A ((amount-of ?sub ?st) ?dst)) ZERO) ; simplification
			(> (A (pressure ?src)) (A (pressure ?dst))))
  :RELATIONS ((Quantity (flow-rate ?self))
	      (> (A (flow-rate ?self)) zero)
	      (Qprop (flow-rate ?self) (pressure ?src))
	      (Qprop- (flow-rate ?self) (pressure ?dst)))
  :INFLUENCES ((I- ((amount-of ?sub ?st) ?src) (flow-rate ?self))
	       (I+ ((amount-of ?sub ?st) ?dst) (flow-rate ?self))))

;;;; Phase changes

(defprocess (boiling (C-S ?sub liquid ?can)
		     (heat-flow ?ht-src ?hpath (C-S ?sub liquid ?can)))
  :INDIVIDUALS ((?sub (substance ?sub))
		(?can (container ?can)
		      (Contained-Liquid (C-S ?sub liquid ?can)))
		(?hpath (heat-path ?hpath))
		(?ht-src (heat-connection ?hpath ?ht-src (C-S ?sub liquid ?can))))
  :QUANTITY-CONDITIONS ((> (A ((amount-of ?sub liquid) ?can)) zero)
			(Active (heat-flow ?ht-src ?hpath (C-S ?sub liquid ?can)))
			(>= (A (temperature (C-S ?sub liquid ?can)))
			    (A (tboil (C-S ?sub liquid ?can)))))
  :RELATIONS ((quantity (generation-rate ?self))
	      (:IMPLIES (Exists (C-S ?sub gas ?can))
			(= (A (temperature (C-S ?sub gas ?can)))
			   (A (temperature (C-S ?sub liquid ?can)))))
	      (> (A (generation-rate ?self)) zero))
  :INFLUENCES ((I+ ((amount-of ?sub gas) ?can) (generation-rate ?self))
	       (I- ((amount-of ?sub liquid) ?can) (generation-rate ?self))
	       (I- (heat (C-S ?sub liquid ?can))
		   (flow-rate (heat-flow ?ht-src ?hpath (C-S ?sub liquid ?can))))))
