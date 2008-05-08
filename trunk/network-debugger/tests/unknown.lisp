(network-debugger unknown :debugging t :extended? t)

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
(enzyme e3 g3 :unknown)
(enzyme e4 :unknown)

;; Experiments
(experiment
 growth-due-to-unknown
 (a b f)
 :growth? t
 :essential-compounds (a e)
 :knock-outs (g1))

(experiment
 no-growth-due-to-unknown
 (a b)
 :growth? nil
 :essential-compounds (a e))

(experiment
 no-growth-independent-of-unknowns
 (a)
 :growth? nil
 :essential-compounds (a e))

#|
Network Debugger UNKNOWN
Adding reaction R1.
Adding reaction R2.
Adding reaction R3.
Adding reaction R4.
Adding enzyme E1.
Adding enzyme E2.
Adding enzyme E3.
Adding enzyme E4.
Closing network for EXPERIMENT.
Adding experiment GROWTH-DUE-TO-UNKNOWN
Assuming simplify-investigations.
Assuming unknown genes and reaction reversibilities as convenient.
Focusing on experiment GROWTH-DUE-TO-UNKNOWN. Experiment GROWTH-DUE-TO-UNKNOWN is coherent.
Adding experiment NO-GROWTH-DUE-TO-UNKNOWN
Retracting focus on experiment GROWTH-DUE-TO-UNKNOWN.
Focusing on experiment NO-GROWTH-DUE-TO-UNKNOWN. Experiment NO-GROWTH-DUE-TO-UNKNOWN is coherent.
Adding experiment NO-GROWTH-INDEPENDENT-OF-UNKNOWNS
Retracting focus on experiment NO-GROWTH-DUE-TO-UNKNOWN.
Focusing on experiment NO-GROWTH-INDEPENDENT-OF-UNKNOWNS. Experiment NO-GROWTH-INDEPENDENT-OF-UNKNOWNS is coherent.
|#