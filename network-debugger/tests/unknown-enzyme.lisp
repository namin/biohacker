(network-debugger unknown-enzyme :debugging t)

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
 :enzymes (:unknown))

(enzyme e1 g1)
(enzyme e2 g2)
(enzyme e3 g3 g3p)

;; Experiments
(experiment
 growth-due-to-unknown
 (a b f)
 :growth? t
 :essential-compounds (a e)
 :knock-outs (g1))

#|
Network Debugger UNKNOWN-ENZYME
Adding reaction R1.
Adding reaction R2.
Adding reaction R3.
Adding reaction R4.
Adding enzyme E1.
Adding enzyme E2.
Adding enzyme E3.
Closing network for EXPERIMENT.
Adding experiment GROWTH-DUE-TO-UNKNOWN
Assuming simplify-investigations.
Focusing on experiment GROWTH-DUE-TO-UNKNOWN. Experiment GROWTH-DUE-TO-UNKNOWN is not coherent. Needs:
( (NUTRIENT E) )
( (NUTRIENT D)  (NUTRIENT G) )
( (NUTRIENT C)  (NUTRIENT G) )
( (REACTION-ENABLED R1) )
( (REACTION-ENABLED R4) )
( (ENZYME-PRESENT UNKNOWN) )
|#