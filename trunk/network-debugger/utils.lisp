(defun list-of (name els)
  (mapcar
   #'(lambda (el)
     (list name el))
   els))

(defun pp-facts (facts &optional (st t))
  (dolist (fact facts)
    (format st "~A~%" (view-fact fact))))

(defmacro tolog (filename &body body)
  `(with-open-file 
    (file ,filename
	  :direction :output
	  :if-exists :append
	  :if-does-not-exist :create)
    (let ((*standard-output* file))
      ,@body)))