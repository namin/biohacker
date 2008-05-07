(defmacro network-debugger (name
			    &key 
			    (debugging nil)
			    (network-file? t) 
			    (experiment-file? t))
  `(let ((nd (create-nd ',name :debugging ,debugging)))
     (debugging-nd
      "~%Network Debugger ~A" ',name)
     ;; TODO: maybe load network and experiment files
     nd))

(defmacro reaction (name
		    &key
		    reactants
		    products
		    (reversible? :UNKNOWN)
		    (enzymes nil))
  `(ensure-network-open reaction
    (run-assert! '(reaction ,name ,reactants ,products ,reversible? ,enzymes)
	     :NETWORK)))

(defmacro enzyme (name &rest genes)
  `(ensure-network-open enzyme
    (run-assert! '(enzyme ,name ,@genes)
	     :NETWORK)))

(defmacro pathway (name
		   &key
		   reactants
		   products
		   (reversible? nil)
		   (enzymes nil)
		   reactions)
  `(ensure-network-open pathway
    (run-assert! '(pathway ,name ,reactants ,products ,reversible? ,enzymes ,reactions)
	     :NETWORK)))

(defmacro experiment (name 
		      nutrients
		      &key
		      growth?
		      (knock-outs nil)
		      (knock-ins nil)
		      (toxins nil)
		      (bootstrap-compounds nil)
		      essential-compounds)
  `(ensure-network-closed experiment
    (run-assert! '(experiment ,name ,growth? 
			  ,nutrients ,essential-compounds
			  ,bootstrap-compounds ,toxins
			  ,knock-ins ,knock-outs)
	     :EXPERIMENTS)
    (investigate-experiment ',name)))

(defmacro ensure-network-open (demander &rest forms)
  `(if (true? 'network-closed)
     (error (format nil "Cannot add ~A as network closed." ',demander))
     (progn ,@forms)))

(defmacro ensure-network-closed (demander &rest forms)
  `(progn
     (when (unknown? 'network-closed)
       (debugging-nd "~%Closing network for ~A." ',demander)
       (run-assert! 'network-closed :ENSURE))
     ,@forms))

(defun retract-focus ()
  (dolist (fact (fetch '(focus-experiment ?e)))
    (when (already-assumed? fact)
      (retract! fact :INVESTIGATION)
      (debugging-nd
       "~%Retracting focus on experiment ~A." (cadr fact)))))

(defun change-focus-experiment (name)
  (retract-focus)
  (assume! `(focus-experiment ,name) :INVESTIGATION)
  (debugging-nd
   "~%Focusing on experiment ~A." name))

(defun investigate-experiment (name &aux result)
  (when (unknown? 'simplify-investigations) 
    (debugging-nd
     "~%Assuming simplify-investigations.")
    (assume! 'simplify-investigations :INVESTIGATION))
  (change-focus-experiment name)
  (setq result
	(cond ((true? 'experiment-coherent)
	       :COHERENT)
	      ((true? 'experiment-growth)
	       (needs 'experiment-coherent :TRUE '((nutrient ?c) (reaction-enabled ?r))))
	      ((false? 'experiment-growth)
	       (needs 'experiment-coherent :TRUE '((:NOT (gene-on ?g)))))
	      ((t (error "Experiment outcome is unknown!")))))
  (when (nd-debugging *nd*)
    (if (eq :COHERENT result) 
	(format t " Experiment ~A is coherent." name)
      (progn 
	(format t " Experiment ~A is not coherent. Needs:" name)
	(pp-sets result t)))))