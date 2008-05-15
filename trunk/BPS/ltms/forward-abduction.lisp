;; biohacker/trunk/BPS/ltms/forward-abduction.lisp
(defun keep-assuming (fact label patterns &key (debugging t) &aux all-assumed)
  (dolist (pattern patterns)
    (dolist (new-fact (fetch pattern))
      (unless (known? new-fact)
	(when debugging
	  (format t "~%Assuming ~A for forward abduction" new-fact))
	(assume! new-fact :FORWARD-ABDUCTION)
	(push new-fact all-assumed)
	(when (known? fact) ;; should check label as sanity check
	  (return-from keep-assuming all-assumed))))))

(defun needs-forward (fact label patterns &key (debugging t) &aux all-assumed antecedents)
  (setq all-assumed (keep-assuming fact label patterns :debugging debugging))
  (setq antecedents 
	(remove-if-not #'already-assumed? (all-antecedents (signed-view-node (get-tms-node fact)) patterns)))
  (dolist (assumed-fact all-assumed)
    (when debugging
      (format t "~%Retracting ~A after forward abduction" assumed-fact))
    (retract! assumed-fact :FORWARD-ABDUCTION))
  antecedents)