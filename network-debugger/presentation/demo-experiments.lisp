;;biohacker/trunk/network-debugger/presentation/demo-experiments.lisp
;;
(experiment 
 positive 
 (a b d) 
 :growth? t 
 :essential-compounds (a e))

(experiment 
 negative 
 (a) 
 :growth? nil 
 :essential-compounds (a e))

(experiment 
 false-positive 
 (a b f) 
 :growth? nil 
 :essential-compounds (a e) 
 :knock-outs (g1))

(experiment
 false-negative
 (a b)
 :growth? t
 :essential-compounds (a e))

#|
Closing network for EXPERIMENT.
Adding experiment POSITIVE
Assuming simplify-investigations.
Assuming unknown genes and reaction reversibilities as convenient.
Focusing on experiment POSITIVE.
Experiment is consistent with model.
Adding experiment NEGATIVE
Retracting focus on experiment POSITIVE.
Focusing on experiment NEGATIVE.
Experiment is consistent with model.
Adding experiment FALSE-POSITIVE
Retracting focus on experiment NEGATIVE.
Focusing on experiment FALSE-POSITIVE.
Experiment is not consistent with model. Needs:
( (NOT (GENE-ON G4)) )
Adding experiment FALSE-NEGATIVE
Retracting focus on experiment FALSE-POSITIVE.
Focusing on experiment FALSE-NEGATIVE.
Experiment is not consistent with model. Needs:
( (NUTRIENT E) )
( (NUTRIENT D) )
( (REACTION-ENABLED R2) )
( (NUTRIENT F) )
|#