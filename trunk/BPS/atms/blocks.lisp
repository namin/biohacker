;; -*- Mode: Lisp; -*-

;;;; Blocks world rules for ATRE
;;; Last edited: 1/29/93, KDF

;; Copyright (c) 1988-1992 Kenneth D. Forbus, Northwestern
;; University, and Johan de Kleer, Xerox Corporation.  
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(Rule-File "BlocksWorld")

;;; First enforce constraints of the domain.

(rule :INTERN ((not ?fact) ?fact)
      ;; The basic consistency test.
      (rnogood! :NEG-DEF ?fact (not ?fact)))

(rule :INTERN ((on ?obj ?s1) :VAR ?f1
	       (on ?obj ?s2) :VAR ?f2
	       :TEST (not (equal ?s1 ?s2)))
      ;Something cannot be two places at once
      (rnogood! :PLACE-EXCLUSION ?f1 ?f2))

(rule :INTERN ((on ?obj1 ?s) :VAR ?f1
	       (on ?obj2 ?s) :VAR ?f2
	       :TEST (and (not (equal ?obj1 ?obj2))
			  (not (equal ?s 'TABLE))))
;;Only one thing can be on top of a block at any time.
      (rnogood! :TOP-EXCLUSION ?f1 ?f2))

(rule :INTERN ((clear ?obj) :VAR ?f1
	       (on ?other ?obj) :VAR ?f2)
;;Something cannot be clear if something else 
;;   is on top of it.
      (rnogood! :TOP-CLEAR-EXCLUSION ?f1 ?f2))

(rule :INTERN ((on ?a ?b) :VAR ?f1)
      ;; Base case for ABOVE
      (rassert! (above ?a ?b) (:ABOVE-BASE-CASE ?f1)))

(rule :INTERN ((above ?a ?b) :VAR ?f1
	       (above ?b ?c) :VAR ?f2)
      ;; ABOVE is transitive
      (rassert! (above ?a ?c) (:ABOVE-TRANSITIVE ?f1 ?f2)))

(rule :INTERN ((above ?a ?a) :VAR ?f1)
      ;; ABOVE is anti-reflexive
      (rnogood! :ABOVE-ANTIREFLEXIVE  ?f1))

(rule :INTERN ((above ?a ?b) :VAR ?f1
	       (above ?b ?a) :VAR ?f2)
      ;; ABOVE is anti-symmetric
      (rnogood! :ABOVE-ANTISYMMETRIC ?f1 ?f2))

;;;; Defining HOLDING
(rule :INTERN ((holding ?obj) :VAR ?f1
	       (clear ?obj) :VAR ?f2)
      ;if you are holding it then it is not clear
      (rnogood! :CLEAR-HOLDING-EXCLUSION ?f1 ?f2))

(rule :INTERN ((holding ?o1) :VAR ?f1
	       (holding ?o2) :VAR ?f2
	       :TEST (not (equal ?o1 ?o2)))
      ;; You can only hold one thing at a time
      (rnogood! :MULTIPLE-HOLD-EXCLUSION ?f1 ?f2))

(rule :INTERN ((holding ?obj) :VAR ?f1
	       (on ?other ?obj) :VAR ?f2)
      ; You cannot hold a block that has something on it.
      (rnogood! :SINGLE-BLOCK-HOLDING ?f1 ?f2))

(rule :INTERN ((holding ?obj) :VAR ?f1
	       (on ?obj ?other) :VAR ?f2)
;; When you are holding something,
;;     it is not on anything else.
      (rnogood! :HOLDING-IN-AIR ?f1 ?f2))

(rule :INTERN ((hand-empty) :VAR ?f1
	       (holding ?obj) :VAR ?f2)
      ;; Your hand isn't empty if it is holding something
      (rnogood! :EMPTY-HOLDING-MUTEX ?f1 ?f2))

;;;; Operators (adapted from Nilsson)

(defoperator (Pickup ?x)
	     :PRECONDITIONS ((on ?x Table)
			     (clear ?x)
			     (hand-empty))
	     :DELETE-LIST  ((on ?x Table)
			     (clear ?x)
			     (hand-empty))
	     :ADD-LIST ((holding ?x)))

(defoperator (Putdown ?x)
	     :PRECONDITIONS ((holding ?x))
	     :DELETE-LIST ((holding ?x))
	     :ADD-LIST ((on ?x Table)
			(clear ?x)
			(hand-empty)))

(defoperator (Stack ?x ?y)
	     :PRECONDITIONS ((holding ?x)
			     (clear ?y))
	     :TEST (not (eq ?y 'TABLE))
	     :DELETE-LIST ((holding ?x)
			   (clear ?y))
	     :ADD-LIST ((hand-empty)
			(on ?x ?y)
			(clear ?x)))

(defoperator (Unstack ?x ?y)
	     :PRECONDITIONS ((hand-empty)
			     (clear ?x)
			     (on ?x ?y))
	     :TEST (not (eq ?y 'TABLE))
	     :DELETE-LIST ((hand-empty)
			   (clear ?x)
			   (on ?x ?y))
	     :ADD-LIST ((holding ?x)
			(clear ?y)))
