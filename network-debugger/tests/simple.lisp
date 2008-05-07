(network-debugger simple nil :debugging t)

;; Network
(reaction 
 r1 
 :reactants (a b) 
 :products (c g)
 :enzymes (e1)
 :reversible? nil)

(reaction 
 r2
 :reactants (b c)
 :products (d)
 :reversible? t)

(reaction 
 r3
 :reactants (d g)
 :products (e)
 :enzymes (e3))

(reaction 
 r4
 :reactants (b f)
 :products (e)
 :enzymes (e4))

(enzyme e1 g1)
(enzyme e3 g3 g3p)
(enzyme e4 g4)

;; Experiments

(experiment
 false-negative
 (a b)
 :growth? t
 :essential-compounds (a e))

(experiment
 false-positive
 (a b f)
 :growth? nil
 :essential-compounds (a e)
 :knock-outs (g1))

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

#|
Network Debugger SIMPLE
Closing network for EXPERIMENT.
Assuming simplify-investigations.
Focusing on experiment FALSE-NEGATIVE. Experiment FALSE-NEGATIVE is not coherent. Needs:
( (NUTRIENT E) )
( (NUTRIENT F) )
( (NUTRIENT D) )
( (REACTION-ENABLED R2) )
Retracting focus on experiment FALSE-NEGATIVE.
Focusing on experiment FALSE-POSITIVE. Experiment FALSE-POSITIVE is not coherent. Needs:
( (NOT (GENE-ON G4)) )
Retracting focus on experiment FALSE-POSITIVE.
Focusing on experiment POSITIVE. Experiment POSITIVE is coherent.
Retracting focus on experiment POSITIVE.
Focusing on experiment NEGATIVE. Experiment NEGATIVE is coherent.
|#