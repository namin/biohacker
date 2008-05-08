(defmacro network-debugger (name
			    &key 
			    (debugging nil)
			    (extended? nil))
  `(let ((nd (create-nd ',name :debugging ,debugging :extended? ,extended?)))
     (debugging-nd
      "~%Network Debugger ~A" ',name)
     nd))

(defmacro reaction (name
		    &key
		    reactants
		    products
		    (reversible? :UNKNOWN)
		    (enzymes nil))
  `(ensure-network-open reaction
    (assert! '(reaction ,name ,reactants ,products ,reversible? ,enzymes)
	     :NETWORK)
    (debugging-nd
     "~%Adding reaction ~A." ',name)))

(defmacro enzyme (name &rest genes)
  `(ensure-network-open enzyme
    (assert! '(enzyme ,name ,@genes)
	     :NETWORK)
    (debugging-nd
     "~%Adding enzyme ~A." ',name)))

(defmacro pathway (name
		   &key
		   reactants
		   products
		   (reversible? nil)
		   (enzymes nil)
		   reactions)
  `(ensure-network-open pathway
    (assert! '(pathway ,name ,reactants ,products ,reversible? ,enzymes ,reactions)
	     :NETWORK)
    (debugging-nd
     "~%Adding pathway ~A." ',name)))

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
    (assert! '(experiment ,name ,growth? 
			  ,nutrients ,essential-compounds
			  ,bootstrap-compounds ,toxins
			  ,knock-ins ,knock-outs)
	     :EXPERIMENTS)
    (debugging-nd
     "~%Adding experiment ~A" ',name)
    (when (nd-debugging *nd*)
      (run-rules)
      (investigate-experiment ',name))))

(defmacro ensure-network-open (demander &rest forms)
  `(if (nd-network-closed? *nd*)
       (error (format nil "Cannot add ~A as network closed." ',demander))
     (progn ,@forms)))

(defmacro ensure-network-closed (demander &rest forms)
  `(progn
     (when (not (nd-network-closed? *nd*))
       (debugging-nd "~%Closing network for ~A." ',demander)
       (run-rules)
       (assert! 'network-closed :ENSURE)
       (setf (nd-network-closed? *nd*) t)
       (run-rules))
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
	      (t (error "Experiment outcome is unknown!"))))
  (when (nd-debugging *nd*)
    (print-investigation name result))
  result)

(defun print-investigation (name result)
  (if (eq :COHERENT result) 
      (format t " Experiment ~A is coherent." name)
    (progn 
      (format t " Experiment ~A is not coherent. Needs:" name)
      (pp-sets result t))))