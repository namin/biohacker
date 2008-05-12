(experiment
 false-negative
 (a b)
 :growth? t
 :essential-compounds (a e))
#|
Focusing on experiment FALSE-NEGATIVE.
Experiment is not consistent with model. Needs:
( (NUTRIENT E) )
( (NUTRIENT D) )
( (REACTION-ENABLED R2) )
( (NUTRIENT F) )
|#

(assume! '(reaction-enabled r2) :user)
(true? 'experiment-consistent)
; T
(explain 'experiment-consistent)
; ..
(all-antecedents 'experiment-consistent '((reaction-enabled ?r)))
; ((REACTION-ENABLED R3) (REACTION-ENABLED R2) (REACTION-ENABLED R1))
(explicit-reversibility)
; Reactions guessed to be reversible: (R1)
; (R1)
(explicit-gene-expression)
; Genes guessed to be on: (G3 G3P G1)
; (G3 G3P G1)
(retract! '(reaction-enabled r2) :user)
(true? 'experiment-consistent)
; NIL