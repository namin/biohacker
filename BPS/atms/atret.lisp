;; -*- Mode: Lisp; -*- 

;;; Tests for ATRE
;; Last edited: 1/29/93, KDF

;; Copyright (c) 1992 Kenneth D. Forbus, Northwestern
;; University, and Johan de Kleer, the Xerox Corporation
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(proclaim '(special *a*))

(defun atre-test1 (&optional (debugging t))
  (setq *atre* (create-atre "Test ATRE"
			    :DEBUGGING debugging))
  (run-forms '((rule :INTERN ((implies ?ante ?conse)
			      :VAR ?f1 ?ante)
		     (rassert! ?conse (:CE ?f1 ?ante)))
       (assume! '(implies (sentient-robot Robbie)
			  (Human Robbie)) :no-bias)
       (assume! '(implies (human Robbie)
			  (mortal Robbie)) :sigh)
       (assume! '(sentient-robot Robbie) :sort-of))))
#| When run on HAL-9000, it looks like this:
> (atre-test1)

    0 rules run.
    Assuming (IMPLIES (SENTIENT-ROBOT ROBBIE)
      (HUMAN ROBBIE)) via NO-BIAS.
    1 rules run.
    Assuming (IMPLIES (HUMAN ROBBIE) (MORTAL
       ROBBIE)) via SIGH.
    1 rules run.
    Assuming (SENTIENT-ROBOT ROBBIE) via SORT-OF.
    Asserting (HUMAN ROBBIE) via (CE (IMPLIES
     (SENTIENT-ROBOT ROBBIE) (HUMAN ROBBIE)) (SENTIENT-ROBOT
      ROBBIE)).
    Asserting (MORTAL ROBBIE) via (CE (IMPLIES (HUMAN 
     ROBBIE) (MORTAL ROBBIE)) (HUMAN ROBBIE)).
    2 rules run.
NIL
> (show-data *atre*)

6 facts total.
(MORTAL ROBBIE): (E-6)
(HUMAN ROBBIE): (E-5)
(SENTIENT-ROBOT ROBBIE): (E-4)
(IMPLIES (HUMAN ROBBIE) (MORTAL ROBBIE)): (E-3)
(IMPLIES (SENTIENT-ROBOT ROBBIE) (HUMAN ROBBIE)): (E-2)
FALSE: NIL
6
|#

(defun atre-test2 (&optional (debugging t))
  (setq *atre* (create-atre "Test ATRE"
			    :DEBUGGING debugging))
  (run-forms '((rule :INTERN ((implies ?ante ?conse)
			      :VAR ?f1 ?ante)
		     (rassert! ?conse (CE ?f1 ?ante)))
       (assert! '(implies (sentient-robot Robbie)
			  (Human Robbie)) 'no-bias)
       (assert! '(implies (human Robbie)
			  (mortal Robbie)) 'sigh)
       (assume! '(sentient-robot Robbie) 'sort-of))))

#| When run on HAL-9000 it looks like this:
> (atre-test2)

    0 rules run.
    Asserting (IMPLIES (SENTIENT-ROBOT ROBBIE)
       (HUMAN ROBBIE)) via (NO-BIAS).
    1 rules run.
    Asserting (IMPLIES (HUMAN ROBBIE)
       (MORTAL ROBBIE)) via (SIGH).
    1 rules run.
    Assuming (SENTIENT-ROBOT ROBBIE) via SORT-OF.
    Asserting (HUMAN ROBBIE) via (CE (IMPLIES
      (SENTIENT-ROBOT ROBBIE) (HUMAN ROBBIE))
      (SENTIENT-ROBOT ROBBIE)).
    Asserting (MORTAL ROBBIE) via (CE (IMPLIES
      (HUMAN ROBBIE) (MORTAL ROBBIE)) (HUMAN ROBBIE)).
    2 rules run.
NIL
> (show-data *a*)

6 facts total.
(MORTAL ROBBIE): (E-2)
(HUMAN ROBBIE): (E-2)
(SENTIENT-ROBOT ROBBIE): (E-2)
(IMPLIES (HUMAN ROBBIE) (MORTAL ROBBIE)): (E-1)
(IMPLIES (SENTIENT-ROBOT ROBBIE) (HUMAN ROBBIE)): (E-1)
FALSE: NIL
6
|#

(defun atre-test3 (&optional (debugging t))
  (setq *atre* (create-atre "Test ATRE"
			    :DEBUGGING debugging))
  (run-forms
	     '((rule :IN ((implies ?ante ?conse)
			  :VAR ?f1 ?ante)
		     (rassert! ?conse (:CE ?f1 ?ante)))
       (assert! '(implies (sentient-robot Robbie)
			  (Human Robbie)) :no-bias)
       (assert! '(implies (human Robbie)
			  (mortal Robbie)) :sigh)
       (assume! '(sentient-robot Robbie) :sort-of))))
#|
> (atre-test3)

    0 rules run.
    Asserting (IMPLIES (SENTIENT-ROBOT ROBBIE)
      (HUMAN ROBBIE)) via (NO-BIAS).
    1 rules run.
    Asserting (IMPLIES (HUMAN ROBBIE)
       (MORTAL ROBBIE)) via (SIGH).
    1 rules run.
    Assuming (SENTIENT-ROBOT ROBBIE) via SORT-OF.
    Asserting (HUMAN ROBBIE) via (CE (IMPLIES
      (SENTIENT-ROBOT ROBBIE) (HUMAN ROBBIE))
      (SENTIENT-ROBOT ROBBIE)).
    Asserting (MORTAL ROBBIE) via (CE (IMPLIES
      (HUMAN ROBBIE) (MORTAL ROBBIE)) (HUMAN ROBBIE)).
    2 rules run.
NIL
> (show-data *a*)

6 facts total.
(MORTAL ROBBIE): (E-2)
(HUMAN ROBBIE): (E-2)
(SENTIENT-ROBOT ROBBIE): (E-2)
(IMPLIES (HUMAN ROBBIE) (MORTAL ROBBIE)): (E-1)
(IMPLIES (SENTIENT-ROBOT ROBBIE) (HUMAN ROBBIE)): (E-1)
FALSE: NIL
6
|#

(defun atre-test4 (&optional (debugging t))
  (setq *atre* (create-atre "Test ATRE"
			    :DEBUGGING debugging))
  (run-forms
	     '((rule :IMPLIED-BY  ((implies ?ante ?conse)
				   :VAR ?f1 ?ante)
		     (rassert! ?conse (:CE ?f1 ?ante)))
       (assume! '(implies (sentient-robot Robbie)
			  (Human Robbie)) :no-bias)
       (assert! '(implies (human Robbie)
			  (mortal Robbie)) :sigh)
       (assume! '(sentient-robot Robbie) :sort-of)))
  (show-data *atre*)
  (print-envs (atre-atms *atre*)))

#| When run on HAL-9000, result looks like this: 
> (atre-test4)

    0 rules run.
    Assuming (IMPLIES (SENTIENT-ROBOT ROBBIE)
       (HUMAN ROBBIE)) via NO-BIAS.
    1 rules run.
    Asserting (IMPLIES (HUMAN ROBBIE)
      (MORTAL ROBBIE)) via (SIGH).
    1 rules run.
    Assuming (SENTIENT-ROBOT ROBBIE) via SORT-OF.
    0 rules run.
4 facts total.
(SENTIENT-ROBOT ROBBIE): (E-3)
(IMPLIES (HUMAN ROBBIE) (MORTAL ROBBIE)): (E-1)
(IMPLIES (SENTIENT-ROBOT ROBBIE) (HUMAN ROBBIE)): (E-2)
FALSE: NIL
E-1: {}
E-3: {(SENTIENT-ROBOT ROBBIE)}
E-2: {(IMPLIES (SENTIENT-ROBOT ROBBIE) (HUMAN ROBBIE))}
NIL
|#

(defun atre-test4a ()
  (change-focus 
	(environment-of
	 '((implies (sentient-robot Robbie) (Human Robbie))
	   (sentient-robot Robbie))))
  (run-rules)
  (show-data)
  (print-envs (atre-atms *atre*)))

#| When run on HAL-9000, it looks like this:
> (atre-test4a)

    Asserting (HUMAN ROBBIE) via (CE (IMPLIES
      (SENTIENT-ROBOT ROBBIE) (HUMAN ROBBIE))
      (SENTIENT-ROBOT ROBBIE)).
    Asserting (MORTAL ROBBIE) via (CE (IMPLIES
      (HUMAN ROBBIE) (MORTAL ROBBIE)) (HUMAN ROBBIE)).
    4 rules run.
6 facts total.
(MORTAL ROBBIE): (E-4)
(HUMAN ROBBIE): (E-4)
(SENTIENT-ROBOT ROBBIE): (E-3)
(IMPLIES (HUMAN ROBBIE) (MORTAL ROBBIE)): (E-1)
(IMPLIES (SENTIENT-ROBOT ROBBIE) (HUMAN ROBBIE)): (E-2)
FALSE: NIL
E-1: {}
E-3: {(SENTIENT-ROBOT ROBBIE)}
E-2: {(IMPLIES (SENTIENT-ROBOT ROBBIE) (HUMAN ROBBIE))}
E-4: {(IMPLIES (SENTIENT-ROBOT ROBBIE) (HUMAN ROBBIE)),
      (SENTIENT-ROBOT ROBBIE)}
NIL
|#

;;;; Test of contradiction rules
(defun atre-test5 (&optional (debugging t))
  (setq *atre* (create-atre "Test ATRE"
			    :DEBUGGING debugging))
  (run-forms '((rule :INTERN ((implies ?ante ?conse)
			      :VAR ?f1 ?ante)
		     (rassert! ?conse (:CE ?f1 ?ante)))
       (assume! '(sentient Robbie) :sort-of)
       (assume! '(immortal Robbie) :why-not) 	       
       (rule :INTERN ((mortal ?x) :VAR ?f1
		      (immortal ?x) :VAR ?f2)
	(rnogood! :DEFINITION ?f1 ?f2))
       (contradiction-rule
	(environment-of '((sentient Robbie)
			  (immortal Robbie)))
	#'(lambda (env)
	    (format T "~% Poor Robbie!  --> ~A." env))
	*atre*)
       (assert! '(implies (sentient Robbie)
			  (Human Robbie)) :no-bias)
       (assert! '(implies (human Robbie)
			  (mortal Robbie)) :sigh))))
#| When run on Hal-9000, it looks like this:
> (atre-test5)

    0 rules run.
    Assuming (SENTIENT ROBBIE) via SORT-OF.
    0 rules run.
    Assuming (IMMORTAL ROBBIE) via WHY-NOT.
    0 rules run.
    0 rules run.
    0 rules run.
    Asserting (IMPLIES (SENTIENT ROBBIE)
       (HUMAN ROBBIE)) via (NO-BIAS).
    Asserting (HUMAN ROBBIE) via (CE (IMPLIES
       (SENTIENT ROBBIE) (HUMAN ROBBIE)) (SENTIENT ROBBIE)).
    2 rules run.
    Asserting (IMPLIES (HUMAN ROBBIE)
      (MORTAL ROBBIE)) via (SIGH).
    Asserting (MORTAL ROBBIE) via (CE (IMPLIES
      (HUMAN ROBBIE) (MORTAL ROBBIE)) (HUMAN ROBBIE)).
    Asserting FALSE via (DEFINITION (MORTAL ROBBIE)
      (IMMORTAL ROBBIE)).
 Poor Robbie!  --> E-4.
    5 rules run.
NIL
> (print-nogoods (atre-atms *atre*))

E-4:* {(IMMORTAL ROBBIE),(SENTIENT ROBBIE)}
NIL
|#


