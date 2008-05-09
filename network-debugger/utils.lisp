(defun list-of (name els)
  (mapcar
   #'(lambda (el)
     (list name el))
   els))

(defun pp-facts (facts &optional (st t))
  (dolist (fact facts)
    (format st "~A~%" (view-fact fact))))

