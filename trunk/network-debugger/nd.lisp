(defmacro network-debugger (name 
			    &key (network-file? t) 
			    (experiment-file? t))
  `(progn
     (format t "(nd ~A)" ',name)
     (when ,network-file?
       (format t "will load network file")
       (when ,experiment-file?
	 (format t "will load experiment file")))))