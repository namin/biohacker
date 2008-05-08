(network-debugger knock :debugging t)

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
 :enzymes (e2)
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
(enzyme e2 g2)
(enzyme e3 g3 g3p)
(enzyme e4 g4)

;; Experiments

(experiment
 positive
 (a b)
 :growth? t
 :essential-compounds (a e))

(experiment
 knock-out-positive
 (a b f)
 :growth? t
 :essential-compounds (a e)
 :knock-outs (g4))

(experiment
 knock-out-false-positive
 (a b f)
 :growth? nil
 :essential-compounds (a e)
 :knock-outs (g1))

(experiment
 knock-out-false-negative
 (a b)
 :growth? t
 :essential-compounds (a e)
 :knock-outs (g3 g4))

(summarize-findings)

#|
Network Debugger KNOCK
Adding reaction R1.
Adding reaction R2.
Adding reaction R3.
Adding reaction R4.
Adding enzyme E1.
Adding enzyme E2.
Adding enzyme E3.
Adding enzyme E4.
Closing network for EXPERIMENT.
Adding experiment POSITIVE
Assuming simplify-investigations.
Focusing on experiment POSITIVE. Experiment POSITIVE is coherent.
Adding experiment KNOCK-OUT-POSITIVE
Retracting focus on experiment POSITIVE.
Focusing on experiment KNOCK-OUT-POSITIVE. Experiment KNOCK-OUT-POSITIVE is coherent.
Adding experiment KNOCK-OUT-FALSE-POSITIVE
Retracting focus on experiment KNOCK-OUT-POSITIVE.
Focusing on experiment KNOCK-OUT-FALSE-POSITIVE. Experiment KNOCK-OUT-FALSE-POSITIVE is not coherent. Needs:
( (NOT (GENE-ON G4)) )
Adding experiment KNOCK-OUT-FALSE-NEGATIVE
Retracting focus on experiment KNOCK-OUT-FALSE-POSITIVE.
Focusing on experiment KNOCK-OUT-FALSE-NEGATIVE. Experiment KNOCK-OUT-FALSE-NEGATIVE is not coherent. Needs:
( (NUTRIENT E) )
( (REACTION-ENABLED R3) )
( (REACTION-ENABLED R4)  (NUTRIENT F) )
2 positive findings: (KNOCK-OUT-POSITIVE POSITIVE)
0 negative findings: NIL
1 false-negative findings: (KNOCK-OUT-FALSE-NEGATIVE)
1 false-positive findings: (KNOCK-OUT-FALSE-POSITIVE)
|#