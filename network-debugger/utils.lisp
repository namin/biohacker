(defun pp-facts (facts &optional (st t))
  (dolist (fact facts)
    (format st "~A~%" (view-fact fact))))