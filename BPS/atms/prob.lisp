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
    (if (null (cdr xs))
        (car xs)
        (cons '* xs))))

(defun symbolic-+ (&rest xs)
  (let ((xs (remove-if #'(lambda (x) (eql 0 x)) xs)))
    (if (null (cdr xs))
        (car xs)
        (cons '+ xs))))

(defun symbolic-- (&rest xs)
  (if (and (not (null (cdr xs))) (null (cddr xs)) (eql 0 (cadr xs)))
      (car xs)
      (cons '- xs)))

(defun symbolic-/ (&rest xs)
  (cons '/ xs))

(setq
 *symbolic*
 (make-numeric
  :title "symbolic"
  :+ #'symbolic-+
  :* #'symbolic-*
  :- #'symbolic--
  :/ #'symbolic-/))

(defun env-prob (e ps)
  (numeric-env-prob *numeric* e ps))

(defun numeric-env-prob (n e ps)
    (let* ((as (env-assumptions e))
           (kps (mapcar #'(lambda (k) (assoc k ps)) as)))
      (if (some #'(lambda (kp) (not kp)) kps)
          nil
          (apply (numeric-* n) (mapcar #'cdr kps)))))

(defun label-prob (l ps)
  (numeric-label-prob *numeric* l ps))

(defun numeric-label-prob (n l ps)
  (numeric-union-prob n (remove nil (mapcar #'(lambda (e) (numeric-env-prob n e ps)) l))))

(defun choose (vs k)
  (cond
    ((= k 0) '(()))
    ((null vs) '())
    (t
     (append
      (mapcar #'(lambda (x) (cons (car vs) x)) (choose (cdr vs) (- k 1)))
      (choose (cdr vs) k)))))

;; P(A U B) = P(A) + P(B) - P(A)*P(B)
;; P(A U B C) = P(A) + P(B) + P(C) - P(A)*P(B) - P(A)*P(C) - P(B)*P(C) + P(A)*P(B)*P(C)
(defun numeric-union-prob (n vs)
  (numeric-union-prob-iter n vs 1 (length vs)))

(defun numeric-union-prob-iter (n vs k x)
  (let ((r (choose vs k)))
    (if (null r)
        0
        (funcall (numeric-- n)
         (apply (numeric-+ n) (mapcar #'(lambda (x) (apply (numeric-* n) x)) r))
         (numeric-union-prob-iter n vs (+ k 1) x)))))

(defun node-prob (n ps)
  (lnumeric-node-prob *numeric* n ps))

(defun numeric-node-prob (n x ps)
  (numeric-label-prob n (tms-node-label x) ps))

(defun prob-node (node ps &optional (stream t) (prefix ""))
  (numeric-prob-node *numeric* node ps stream prefix))

(defun prob-nodes (atms ps &optional (stream t))
  (numeric-prob-nodes *numeric* atms ps stream))

(defun why-prob-node (node ps &optional (stream t) (prefix ""))
  (numeric-prob-node *numeric* node ps stream prefix))

(defun why-prob-nodes (atms ps &optional (stream t))
  (dolist (n (reverse (atms-nodes atms))) (why-prob-node n ps stream)))

(defun numeric-prob-node (n node ps &optional (stream t) (prefix ""))
  (format stream "~%<~A~A," prefix (tms-node-datum node))
  (format stream "~2$" (numeric-node-prob n node ps))
  (format stream ">"))

(defun numeric-prob-nodes (n atms ps &optional (stream t))
  (dolist (x (reverse (atms-nodes atms))) (numeric-prob-node n x ps stream)))

(defun numeric-why-prob-node (n node ps &optional (stream t) (prefix ""))
  (format stream "~%<~A~A," prefix (tms-node-datum node))
  (format stream "~2$:{" (numeric-node-prob n node ps))
  (dolist (e (tms-node-label node))
    (let ((pe (numeric-env-prob n e ps)))
      (when pe (format stream "~2$:" pe))
      (env-string e stream)))
  (format stream "}>"))

(defun numeric-why-prob-nodes (n atms ps &optional (stream t))
  (dolist (x (reverse (atms-nodes atms))) (numeric-why-prob-node n x ps stream)))
