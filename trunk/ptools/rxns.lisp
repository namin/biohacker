(load "export-rxns.lisp")
(setq *rxns* (collect-rxns (all-rxns :small-molecule) #'balanced-rxn-p))
(setq *catalyses* (collect-catalyses (all-rxns :small-molecule) #'balanced-rxn-p))
(setq *enzymes* (collect-enzymes (all-rxns :small-molecule) #'balanced-rxn-p))
