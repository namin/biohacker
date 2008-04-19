;;; -*- Mode: LISP; Syntax: Common-lisp; -*-

;;;; Examples for Justification-based TMS
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defun get-node (datum jtms)
  (dolist (node (jtms-nodes jtms))
    (if (equal datum (tms-node-datum node)) (return node))))

(defun get-justification (num jtms)
  (dolist (just (jtms-justs jtms))
    (if (= num (just-index just)) (return just))))

(declare (special na nb nc nd ne nf ng contra *jtms*))

(defun ex1 ()
  (setq *jtms* (create-jtms "Simple Example" :debugging T)
	na (tms-create-node *jtms* 'a :assumptionp T)
	nb (tms-create-node *jtms* 'b :assumptionp T)
	nc (tms-create-node *jtms* 'c :assumptionp T)
	nd (tms-create-node *jtms* 'd :assumptionp T)
	ne (tms-create-node *jtms* 'e :assumptionp T)
	nf (tms-create-node *jtms* 'f :assumptionp T)
	ng (tms-create-node *jtms* 'g :assumptionp T))
  (justify-node 'j1 nf (list na nb))
  (justify-node 'j2 ne (list nb nc))
  (justify-node 'j3 ng (list na ne))
  (justify-node 'j4 ng (list nd ne))
  (enable-assumption na)
  (enable-assumption nb)
  (enable-assumption nc)
  (enable-assumption nd))

(defun ex2 () ;; uses Ex1 to test the contradiction stuff.
  (setq contra (tms-create-node *jtms* 'Loser :contradictoryp T))
  (justify-node 'j5 contra (list ne nf)))

(defun ex3 ()
  (setq *jtms* (create-jtms "Multiple support example")
	assumption-a (tms-create-node *jtms* 'A :assumptionp T)
	assumption-c (tms-create-node *jtms* 'C :assumptionp T)
	assumption-e (tms-create-node *jtms* 'E :assumptionp T)
	node-h (tms-create-node *jtms* 'h))
  (enable-assumption assumption-a)
  (enable-assumption assumption-c)
  (enable-assumption assumption-e)
  (justify-node 'R1 node-h (list assumption-c assumption-e))
  (setq node-g (tms-create-node *jtms* 'g))
  (justify-node 'R2 node-g (list assumption-a assumption-c))
  (setq contradiction (tms-create-node *jtms*
				       'CONTRADICTION :contradictoryp T))
  (justify-node 'R3 contradiction (list node-g)))

