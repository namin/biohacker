(defstruct (nd (:PRINT-FUNCTION nd-print-procedure))
  title				  ; Pretty name
  ltre				  ; Pointer to its LTRE
  (debugging nil)		  ; Show basic operations
  (log nil)    		          ; File to log progress and results.
  rules   		       	  ; Which rules to use:
				  ; :just-reactions
				  ; :extended-reactions (support for :unknown genes and reversible reactions)
				  ; :just-pathways
  (network-closed? nil)	          ; Whether reactions, enzymes and pathways can still be added
  (findings nil)                  ; An association list of (experiment-name . finding)
  (abducting nil)                 ; Whether abducting during investigation
  (growth-patterns nil)
  (no-growth-patterns nil)
  )

(defun nd-print-procedure (nd st ignore)
  (declare (ignore ignore))
  (format st "<network-debugger: ~A (~A)>" (nd-title nd) (if (nd-network-closed? nd) 'closed 'open)))

(defvar *ND* nil) ;; Default ND

(defmacro with-ND (nd &rest forms)
  `(let ((*ND* ,nd)) 
     (with-LTRE ,(nd-ltre nd) ,@forms)))

(defun In-ND (nd) 
  (setq *ND* nd)
  (In-LTRE (nd-ltre nd)))

(defmacro debugging-nd (msg &rest args)
  `(when (nd-debugging *ND*) (format t ,msg  ,@args)))

(defmacro tolog (filename &body body)
  `(with-open-file 
    (file ,filename
	  :direction :output
	  :if-exists :append
	  :if-does-not-exist :create)
    (let ((*standard-output* file))
      ,@body)))

(defmacro when-logging-nd (&body body)
  `(when (nd-log *ND*)
     (tolog
      (nd-log *ND*)
      ,@body)))

(defmacro debugging-or-logging-nd (msg &rest args)
  `(progn
     (debugging-nd
       ,msg ,@args)
     (logging-nd
       ,msg ,@args)))

(defmacro logging-nd (msg &rest args)
  `(when-logging-nd
    (format t ,msg ,@args)))

(defmacro when-debugging-or-logging-nd (&body body)
  `(progn
     (when (nd-debugging *nd*)
       ,@body)
     (when-logging-nd
      ,@body)))

(defun create-nd (title &key debugging log rules abducting growth-patterns no-growth-patterns)
  (unless rules
    (setq rules :just-reactions))
   (let ((nd (make-nd
	      :TITLE title 
	      :LTRE (create-ltre (list :LTRE-OF title))
	      :DEBUGGING debugging
	      :ABDUCTING abducting
	      :LOG log
	      :rules rules
	      :growth-patterns growth-patterns
	      :no-growth-patterns no-growth-patterns)))
     (setq *ND* nd)
     (change-ltms 
      (ltre-ltms (nd-ltre nd)) 
      :node-string 'nd-node-string)
     (load
      (ecase rules
	((:just-reactions) *nd-rules-file*)
	((:extended-reactions) *nd-extended-rules-file*)
	((:just-pathways) *nd-pathway-rules-file*)))
     nd))

(defun change-nd (nd &key (debugging nil debugging?) (log nil log?) (abducting nil abducting?) (growth-patterns nil growth-patterns?) (no-growth-patterns nil no-growth-patterns?))
  (when debugging? (setf (nd-debugging nd) debugging))
  (when log? (setf (nd-log nd) log))
  (when abducting? (setf (nd-abducting nd) abducting))
  (when growth-patterns? (setf (nd-growth-patterns nd) growth-patterns))
  (when no-growth-patterns? (setf (nd-no-growth-patterns nd) no-growth-patterns)))

(defun view-fact (fact)
  (when (listp fact)
    (setq 
     fact
     (cond ((eq 'experiment (car fact))
	    `(experiment ,(cadr fact) ,(caddr fact)))
	   ((find (car fact) '(reaction pathway))
	    `(,(car fact) 
	      ,(cadr fact) 
	      ,(caddr fact)
	      ,(ecase (cadr (cdddr fact))
		 ((nil) '-->)
		 ((t) '<->)
		 ((:UNKNOWN) '?->))
	      ,(cadddr fact)))
	   (t fact))))
  fact)

(defun nd-node-string (node)
  (format nil "~A" (view-fact (view-node node))))

(defmacro run-assert! (fact just)
  `(progn
     (assert! ,fact ,just)
     (run-rules-logging)))

(defun n-rules-run ()
  (ltre-rules-run (nd-ltre *nd*)))

(defun run-rules-logging (&aux n-before n-after)
  (setq n-before (n-rules-run))
  (run-rules)
  (setq n-after (n-rules-run))
  (logging-nd
   "~%~A rules run." (- n-after n-before)))