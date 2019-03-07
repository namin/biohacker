;; -*- MODE: LISP; -*-

;;;; Simple loader for Building Problem Solvers

;; Source code type

(defvar *default-source-type*
  "lisp")

;; compiled file type
(defvar *default-bin-type*
  "fasl")

;; Where to get stuff
(defvar *default-pathname* 
  *bps-home*
)

(defun load-files (file-list &optional (path *default-pathame*)
			     (type *default-bin-type*))
  (dolist (file file-list)
	  (load (merge-pathnames path
				 (concatenate 'string file "." type)))))

(defun compile-files (file-list
		       &optional (path *default-pathname*))
  (dolist (file file-list)
    (format t "~% Compiling ~A..."
	    (merge-pathnames path 
			     (concatenate 'string file "."
					  *default-source-type*)))
	  (compile-file
	    (merge-pathnames path 
			     (concatenate 'string file "."
					  *default-source-type*))
	    :OUTPUT-FILE
	    (merge-pathnames path
			     (concatenate 'string file "."
					  *default-bin-type*)))))

(defun load-from (file path)
  (load (concatenate 'string path file)))

;;;; Compiling and loading files

(defun compile-load-files (file-list
			    &optional (path *default-pathname*)
			   (pre-load? t) 
			    &aux out-path)
  (dolist (file file-list)
	  (setq out-path 
	    (merge-pathnames path
			     (concatenate 'string file "."
					  *default-bin-type*)))
    	    (when pre-load?
	      (load (merge-pathnames path 
				     (concatenate 'string file "."
						  *default-source-type*))))
	  (compile-file
	    (merge-pathnames path 
			     (concatenate 'string file "."
					  *default-source-type*))
	    :OUTPUT-FILE out-path)
	  (load out-path)))
