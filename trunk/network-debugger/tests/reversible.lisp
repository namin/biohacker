(network-debugger reversible :debugging t :extended? t)

;; Network
(reaction 
 r1 
 :reactants (c g)
 :products (a b)
 :enzymes (e1))

(reaction 
 r2
 :reactants (b c)
 :products (d)
 :enzymes (e2))

(reaction 
 r3
 :reactants (d g)
 :products (e)
 :enzymes (e3))

(reaction 
 r4
 :reactants (e)
 :products (b f)
 :enzymes (g4))

(enzyme e1 g1)
(enzyme e2 g2)
(enzyme e3 g3 g3p)
(enzyme e4 g4)

;; Experiments
(experiment
 growth-if-r1-reversible
 (a b)
 :growth? t
 :essential-compounds (a e)
 :knock-outs (g4))

(experiment
 no-growth-if-r4-not-reversible
 (a b f)
 :growth? nil
 :essential-compounds (a e)
 :knock-outs (g1))

#|
Network Debugger REVERSIBLE
Adding reaction R1.
Adding reaction R2.
Adding reaction R3.
Adding reaction R4.
Adding enzyme E1.
Adding enzyme E2.
Adding enzyme E3.
Adding enzyme E4.
Closing network for EXPERIMENT.
Adding experiment GROWTH-IF-R1-REVERSIBLE
Assuming simplify-investigations.
Assuming unknown genes and reaction reversibilities as convenient.
Focusing on experiment GROWTH-IF-R1-REVERSIBLE. Experiment GROWTH-IF-R1-REVERSIBLE is coherent.
Adding experiment NO-GROWTH-IF-R4-NOT-REVERSIBLE
Retracting focus on experiment GROWTH-IF-R1-REVERSIBLE.
Focusing on experiment NO-GROWTH-IF-R4-NOT-REVERSIBLE. Experiment NO-GROWTH-IF-R4-NOT-REVERSIBLE is coherent.
|#