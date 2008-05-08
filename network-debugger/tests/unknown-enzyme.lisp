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
