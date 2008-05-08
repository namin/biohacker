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
  `(ensure-network-closed 
    experiment
    (assert! '(experiment 
	       ,name ,growth? 
	       ,nutrients ,essential-compounds
	       ,bootstrap-compounds ,toxins
	       ,knock-ins ,knock-outs)
	     :EXPERIMENTS)
    (debugging-nd
     "~%Adding experiment ~A" ',name)
    (run-rules)
    (investigate-experiment ',name)))

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

(defun investigate-experiment (name &aux result cache)
  (when (unknown? 'simplify-investigations) 
    (debugging-nd
     "~%Assuming simplify-investigations.")
    (assume! 'simplify-investigations :INVESTIGATION))
  (when (and (nd-extended? *nd*) (unknown? 'assume-unknowns-as-convenient)) 
    (debugging-nd
     "~%Assuming unknown genes and reaction reversibilities as convenient.")
    (assume! 'assume-unknowns-as-convenient :INVESTIGATION))  
  (change-focus-experiment name)
  (setq cache
	(assoc name (nd-findings *nd*)))
  (setq result
	(cdr cache))
  (unless cache
    (setq result
	  (cond ((true? 'experiment-coherent)
		 :COHERENT)
		((true? 'experiment-growth)
		 (needs 'experiment-coherent :TRUE '((nutrient ?c) (reaction-enabled ?r) (enzyme-present ?x))))
		((false? 'experiment-growth)
		 (needs 'experiment-coherent :TRUE '((:NOT (gene-on ?g)))))
		(t (error "Experiment outcome is unknown!"))))
    (setf (nd-findings *nd*) (acons name result (nd-findings *nd*))))
  (when (nd-debugging *nd*)
    (print-investigation name result))
  result)

(defun print-investigation (name result)
  (if (eq :COHERENT result) 
      (format t " Experiment ~A is coherent." name)
    (progn 
      (format t " Experiment ~A is not coherent. Needs:" name)
      (pp-sets result t))))

(defun filter-findings-by-growth (growth? &optional (findings (nd-findings *nd*)))
  (remove-if-not
   #'(lambda (name)
       (fetch `(experiment ,name ,growth? . ?x)))
   findings
   :key #'car))

(defun filter-findings-by-coherence (coherent? &optional (findings (nd-findings *nd*)))
  (remove-if-not
   (if coherent?
       #'(lambda (result) (eq result :COHERENT))
     #'(lambda (result) (not (eq result :COHERENT))))
   findings
   :key #'cdr))

(defun summarize-findings (&aux positive negative false-negative false-positive growth no-growth summary)
  (setq growth (filter-findings-by-growth t))
  (setq no-growth (filter-findings-by-growth nil))
  (setq positive (filter-findings-by-coherence t growth))
  (setq negative (filter-findings-by-coherence t no-growth))
  (setq false-negative (filter-findings-by-coherence nil growth))
  (setq false-positive (filter-findings-by-coherence nil no-growth))
  (setq summary (list positive negative false-negative false-positive))
  (when (nd-debugging *nd*)
    (print-summary summary))
  summary)

(defun print-summary-line (about line)
  (format t "~%~A ~A findings: ~A" (list-length line) about (mapcar #'car line)))

(defun print-summary (summary)
  (print-summary-line "positive" (car summary))
  (print-summary-line "negative" (cadr summary))
  (print-summary-line "false-negative" (caddr summary))
  (print-summary-line "false-positive" (cadddr summary)))

(defun fix-for-experiment (name &aux cache)
  (if (setq cache (assoc name (nd-findings *nd*)))
      (cdr cache)
    :UNKNOWN-EXPERIMENT))
