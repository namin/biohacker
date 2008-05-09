(network-debugger simple :debugging t :log "simple-stats.log")

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

(experiment
 false-negative
 (a b)
 :growth? t
 :essential-compounds (a e))

(summarize-findings)