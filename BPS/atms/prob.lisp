(defstruct (numeric (:PRINT-FUNCTION print-numeric))
  (title nil)
  (+ nil)
  (* nil)
  (- nil)
  (/ nil))

(defun print-numeric (causal stream ignore)
  (declare (ignore ignore))
  (format stream "#<numeric: ~A>" (numeric-title causal)))

(setq
 *numeric*
 (make-numeric
  :title "numeric"
  :+ #'+
  :* #'*
  :- #'-
  :/ #'/))

(defun symbolic-* (&rest xs)
  (let ((xs (remove-if #'(lambda (x) (eql 1 x)) xs)))
    (if (null xs)
        1
        (if (null (cdr xs))
            (car xs)
            (cons '* xs)))))

(defun symbolic-+ (&rest xs)
  (let ((xs (remove-if #'(lambda (x) (eql 0 x)) xs)))
    (if (null xs)
        0
        (if (null (cdr xs))
            (car xs)
            (cons '+ xs)))))

(defun symbolic-- (&rest xs)
  (if (and (not (null (cdr xs))) (null (cddr xs)) (eql 0 (cadr xs)))
      (car xs)
      (cons '- xs)))

(defun symbolic-/ (&rest xs)
  (if (and (= 2 (length xs)) (equal (car xs) (cadr xs)))
      1 ;; ignoring div by 0
      (cons '/ xs)))

(setq
 *symbolic*
 (make-numeric
  :title "symbolic"
  :+ #'symbolic-+
  :* #'symbolic-*
  :- #'symbolic--
  :/ #'symbolic-/))
