(defstruct (nd (:PRINT-FUNCTION nd-print-procedure))
  title
  ltre
)

(defun nd-print-procedure (nd st ignore)
  (declare (ignore ignore))
  (format st "<network-debugger: ~A>" (nd-title l)))

(defvar *ND* nil) ;; Default ND

(defmacro with-ND (nd &rest forms)
  `(let ((*ND* ,nd)) 
     (with-LTRE ,(nd-ltre nd) ,@forms)))

(defun In-ND (nd) 
  (setq *ND* nd)
  (In-LTRE (nd-ltre nd)))

(defmacro network-debugger (name 
			    &key (network-file? t) 
			    (experiment-file? t))
  `(progn
     (format t "(nd ~A)" ',name)
     (when ,network-file?
       (format t "will load network file")
       (when ,experiment-file?
	 (format t "will load experiment file")))))