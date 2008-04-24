(in-package :ecocyc)

(defun collect-rxns (rxn-list filter-p)
  (loop for rxn in rxn-list
     when (funcall filter-p rxn)
     collect (translate-rxn-to-tms rxn)))

(defun collect-catalyses (rxn-list filter-p)
  (loop for rxn in rxn-list
       when (funcall filter-p rxn)
       collect (translate-catalysis-to-tms rxn)))

(defun collect-enzymes (rxn-list filter-p)
  (loop for enzyme in (remove-duplicates 
		       (loop for rxn in rxn-list
			  when (funcall filter-p rxn)
			  append (enzymes-of-reaction rxn)))
     collect (translate-enzyme-to-tms enzyme)))

(defun translate-catalysis-to-tms (rxn)
  `(catalyze ,(get-frame-name rxn)
	     ,@(enzymes-of-reaction rxn)))

(defun translate-enzyme-to-tms (enzyme)
  `(enzyme ,(get-frame-name enzyme)
	   ,@(genes-of-protein enzyme)))



(defun translate-rxn-to-tms (rxn)
  `(reaction ,(get-frame-name rxn) ,(get-slot-values rxn 'left) ,@(get-slot-values rxn 'right)))

(defun substrate-not-frame-p (rxn)
  (loop for substrate in (substrates-of-reaction rxn)
       if (not (coercible-to-frame-p substrate))
       return substrate))

(defun balanced-rxn-p (rxn)
  (and (coercible-to-frame-p rxn)
       (not (substrate-not-frame-p rxn))
       (atomic-balanced-p rxn 'C)
       (atomic-balanced-p rxn 'N)
       (atomic-balanced-p rxn 'S)
       (atomic-balanced-p rxn 'P)))

(defun get-stoichiometry (rxn side participant)
  (let ((coeff (get-value-annot rxn side participant 'coefficient)))
    (if coeff
	coeff
	1)))

(defun get-atom-number (molecule the-atom)
  (or (loop for (atom num) in (get-slot-values molecule 'chemical-formula)
	 when (eq atom the-atom)
	 return num)
      0))

(defun get-num-atoms-on-side (rxn side the-atom)
  (loop for participant in (get-slot-values rxn side)
     sum (* (get-stoichiometry rxn side participant) 
	    (get-atom-number participant the-atom))))

(defun atomic-balanced-p (rxn the-atom)
  (let ((reactant-atoms 
	 (get-num-atoms-on-side rxn 'left the-atom))
	(product-atoms
	 (get-num-atoms-on-side rxn 'right the-atom)))
    (= 0 (- reactant-atoms product-atoms))))
    
	
	      