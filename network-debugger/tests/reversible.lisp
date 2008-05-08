(network-debugger unknown-enzyme :debugging t)

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
 :reactants (b f)
 :products (e)
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
