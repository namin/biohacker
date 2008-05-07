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
  `(assert! '(reaction ,name ,reactants ,products ,reversible? ,enzymes)
	    :NETWORK))

(defmacro enzyme (name &rest genes)
  `(assert! '(enzyme ,name ,@genes)
	    :NETWORK))

(defmacro pathway (name
		   &key
		   reactants
		   products
		   (reversible? nil)
		   (enzymes nil)
		   reactions)
  `(assert! '(pathway ,name ,reactants ,products ,reversible? ,enzymes ,reactions)
	    :NETWORK))

(defmacro experiment (name 
		      nutrients
		      &key
		      growth?
		      (knock-outs nil)
		      (knock-ins nil)
		      (toxins nil)
		      (bootstrap-compounds nil)
		      essential-compounds)
  `(assert! '(experiment ,name ,growth? 
			 ,nutrients ,essential-compounds
			 ,bootstrap-compounds ,toxins
			 ,knock-ins ,knock-outs)
	    :EXPERIMENTS))