;; biohacker/trunk/network-debugger/utils.lisp
(defun list-of (name els)
  (mapcar
   #'(lambda (el)
     (list name el))
   els))

(defun pp-facts (facts &optional (st t))
  (dolist (fact facts)
    (format st "~A~%" (view-fact fact))))

(defmacro tofile (filename &body body)
  `(with-open-file (file ,filename
		    :direction :output
		    :if-exists :supersede)
     (let ((*standard-output* file))
       ,@body) ))

(defun pathway-tools-file (filename lst)
(tofile filename (loop for el in lst do (format t "~A	10~%" el))))
