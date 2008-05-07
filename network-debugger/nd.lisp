(defmacro network-debugger (name
			    &key 
			    (debugging nil)
			    (network-file? t) 
			    (experiment-file? t))
  `(let ((nd (create-nd ',name :debugging ,debugging)))
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
	     :EXPERIMENTS)))

(defmacro ensure-network-open (demander &rest forms)
  `(if (true? 'network-closed)
     (error (format nil "Cannot add ~A as network closed." ',demander))
     (progn ,@forms)))

(defmacro ensure-network-closed (demander &rest forms)
  `(progn
     (when (unknown? 'network-closed)
       (debugging-nd "Closing network for ~A." ',demander)
       (run-assert! 'network-closed :ENSURE))
     ,@forms))