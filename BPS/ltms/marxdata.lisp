;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                             -*-
;;;; ------------------------------------------------------------------------------
;;;; File name: marxdata.lsp
;;;;    System: Rules for the Marx Brothers Problem
;;;;   Version: 1.0
;;;;    Author: Kenneth D. Forbus
;;;;   Created: Apr 18, 1996
;;;;  Modified: forbus on Thurs Apr 18 10:10:44 1996
;;;;   Purpose: Data for figuring out which Marx Brother was which
;;;; ------------------------------------------------------------------------------

(in-package :COMMON-LISP-USER)

;; Here are the constraints, from page 648-649 of Building Problem Solvers:
;; 1. The pianist, harpist, and talker are distinct brothers.
;; 2. The brother who is fond of money is distinct from the one
;;    who is fond of gambling, who is also distinct from the one
;;    who is fond of animals.
;; 3. The one who likes to talk doesn't like gambling.
;; 4. The one who likes animals plays the harp.
;; 5. Groucho hates animals.
;; 6. Harpo is always silent.
;; 7. Chico plays the piano.

;; We could simply write a large set of rules that trigger on pairs of 
;; assertions to implement these constraints, but that isn't very elegant
;; and provides many opportunities for error.  Instead, we use explicit
;; higher-order relations to state the constraints in a concise manner,
;; and use rules that implement the semantics of these constraints to do
;; the work.  This makes it easier to write and debug, as well as putting more
;; of the knowledge we extract from the constraints in the system explicitly,
;; so that they can be reasoned about.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description of the constraints, formally

;; 1. The pianist, harpist, and talker are distinct brothers.

(rassert! (pairwise-nogood plays-piano plays-harp))
(rassert! (pairwise-nogood plays-piano smooth-talker))
(rassert! (pairwise-nogood plays-harp smooth-talker))

;; 2. The brother who is fond of money is distinct from the one
;;    who is fond of gambling, who is also distinct from the one
;;    who is fond of animals.

(rassert! (pairwise-nogood likes-money likes-gambling))
(rassert! (pairwise-nogood likes-gambling likes-animals))

;; 3. The one who likes to talk doesn't like gambling.

(rassert! (pairwise-nogood smooth-talker likes-gambling))

;; 4. The one who likes animals plays the harp.

(rassert! (same-entity likes-animals plays-harp))

;; 5. Groucho hates animals.

(rassert! (:not (likes-animals groucho)))

;; 6. Harpo is always silent.

(rassert! (:not (smooth-talker harpo)))

;; 7. Chico plays the piano.

(rassert! (plays-piano chico))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Implementing the higher-order relations

(rule ((:true (pairwise-nogood ?attribute1 ?attribute2) :var ?hor)
       (:true (?attribute1 ?obj) :var ?f1)
       (:true (?attribute2 ?obj) :var ?f2))
   (rassert! (:not (:and ?hor ?f1 ?f2))))

(rule ((:true (same-entity ?attribute1 ?attribute2) :var ?hor)
       (:true (?attribute1 ?obj) :var ?f1))
   (rassert! (:implies (:and ?hor ?f1) (?attribute2 ?obj))))

