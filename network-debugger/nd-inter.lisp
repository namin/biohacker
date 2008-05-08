(defstruct (nd (:PRINT-FUNCTION nd-print-procedure))
  title                   ; Pretty name
  ltre                    ; Pointer to its LTRE
  (debugging nil)         ; Show basic operations
  (log nil)               ; File to log progress and results.
  (extended? nil)         ; Whether to use extended rules (with support for :unknown genes and reversible reactions) or basic rules
  (network-closed? nil)   ; Whether reactions, enzymes and pathways can still be added
  (findings nil)          ; An association list of (experiment-name . investigation-result)
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

(defun create-nd (title &key debugging log extended?)
   (let ((nd (make-nd
	      :TITLE title 
	      :LTRE (create-ltre (list :LTRE-OF title))
	      :DEBUGGING debugging
	      :LOG log
	      :EXTENDED? extended?)))
     (setq *ND* nd)
     (change-ltms 
      (ltre-ltms (nd-ltre nd)) 
      :node-string 'nd-node-string)
     (if extended?
	 (load *nd-extended-rules-file*)
       (load *nd-rules-file*))
     nd))

(defun change-nd (nd &key (debugging nil debugging?))
  (if debugging? (setf (nd-debugging nd) debugging)))

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