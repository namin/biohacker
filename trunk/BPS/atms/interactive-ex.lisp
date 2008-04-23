(setq *atms* (create-atms "Simple Example"))
(setq assumption-a (tms-create-node *atms* "A" :assumptionp t)
      assumption-c (tms-create-node *atms* "C" :assumptionp t)
      assumption-e (tms-create-node *atms* "E" :assumptionp t))

(setq node-h (tms-create-node *atms* "h"))
(justify-node "R1" node-h (list assumption-c assumption-e))
(why-node node-h)

(setq node-g (tms-create-node *atms* "g"))
(justify-node "R2" node-g (list assumption-a assumption-c))
(setq contradiction (tms-create-node *atms* 'contradiction :contradictoryp t))
(justify-node "R3" contradiction (list node-g))
(why-node node-g)

(mapc 'print-env (interpretations *atms* nil (atms-assumptions *atms*)))

(mapcar #'(lambda (le)
	    (in-node? node-h le)) 
	(interpretations *atms* nil (atms-assumptions *atms*)))



