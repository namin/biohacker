(setq *log-file* "simple-stats-log.txt")

(tolog *log-file* (format t "~%creating network..."))

(network-debugger simple :debugging nil)

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

(tolog *log-file* (format t " done."))

;(change-ltre (nd-ltre *nd*) :debugging t)

(tolog *log-file* (format t "~%running rules..."))
(run-rules)
(setq network-rules-run (ltre-rules-run (nd-ltre *nd*)))
(tolog *log-file* (format t " done (~A rules run)." network-rules-run))

(tolog *log-file* (format t "~%closing network..."))
(assert! 'network-closed :ENSURE)
(setf (nd-network-closed? *nd*) t)
(run-rules)
(setq closing-rules-run (- (ltre-rules-run (nd-ltre *nd*)) network-rules-run))
(tolog *log-file* (format t " done (~A rules run)." closing-rules-run))


(tolog *log-file* (format t "~%running experiment ..."))
(experiment
 false-negative
 (a b)
 :growth? t
 :essential-compounds (a e))
(run-rules)
(setq experiment-rules-run (- (ltre-rules-run (nd-ltre *nd*)) network-rules-run closing-rules-run))
(tolog *log-file* (format t " done (~A rules run)." experiment-rules-run))

(setq name 'false-negative)
(tolog *log-file* (format t "~%investigating experiment..."))
(setq result (investigate-experiment name))
(tolog *log-file* 
       (format t " done. result:")
       (print-investigation name result))
