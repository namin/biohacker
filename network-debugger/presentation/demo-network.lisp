(network-debugger demo :debugging t :abducting t :rules :extended-reactions)

(reaction r1 :reactants (c g) :products (a b) :enzymes (e1))
(reaction r2 :reactants (b c) :products (d))
(reaction r3 :reactants (d g) :products (e) :enzymes (e3))
(reaction r4 :reactants (b f) :products (e) :enzymes (e4))

(enzyme e1 g1)
(enzyme e3 g3 g3p)
(enzyme e4 g4)

;; in reality:
;; - r1 should be reversed
;; - r2 has no catalysts in the model, but it is enabled in reality
;; - r4 shouldn't be enabled

#|
Network Debugger DEMO
Adding reaction R1.
Adding reaction R2.
Adding reaction R3.
Adding reaction R4.
Adding enzyme E1.
Adding enzyme E3.
Adding enzyme E4.
|#