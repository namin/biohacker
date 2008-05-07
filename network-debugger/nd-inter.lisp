(defstruct (nd (:PRINT-FUNCTION nd-print-procedure))
  title                   ; Pretty name
  ltre                    ; Pointer to its LTRE
  (debugging nil)         ; Show basic operations
)

(defun nd-print-procedure (nd st ignore)
  (declare (ignore ignore))
  (format st "<network-debugger: ~A>" (nd-title nd)))

(defvar *ND* nil) ;; Default ND

(defmacro with-ND (nd &rest forms)
  `(let ((*ND* ,nd)) 
     (with-LTRE ,(nd-ltre nd) ,@forms)))

(defun In-ND (nd) 
  (setq *ND* nd)
  (In-LTRE (nd-ltre nd)))

(defmacro debugging-nd (msg &rest args)
  `(when (nd-debugging *ND*) (format t ,msg  ,@args)))

(defun create-nd (title &key debugging)
   (let ((nd (make-nd
	      :TITLE title 
	      :LTRE (create-ltre (list :LTRE-OF title))
	      :DEBUGGING debugging)))
     (setq *ND* nd)
     (load *nd-rules-file*)
     nd))

(defun change-nd (nd &key (debugging nil debugging?))
  (if debugging? (setf (nd-debugging nd) debugging)))

