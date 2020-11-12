(defstruct (numeric (:PRINT-FUNCTION print-numeric))
  (title nil)
  (+ nil)
  (* nil)
  (- nil)
  (/ nil))

(defun print-causal (causal stream ignore)
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
    (let* ((as (env-assumptions e))
           (kps (mapcar #'(lambda (k) (assoc k ps)) as)))
      (if (some #'(lambda (kp) (not kp)) kps)
          nil
          (apply #'* (mapcar #'cdr kps)))))

(defun symbolic-env-prob (e ps)
    (let* ((as (env-assumptions e))
           (kps (mapcar #'(lambda (k) (assoc k ps)) as)))
      (if (some #'(lambda (kp) (not kp)) kps)
          nil
          (apply #'symbolic-* (mapcar #'cdr kps)))))

(defun numeric-env-prob (n e ps)
    (let* ((as (env-assumptions e))
           (kps (mapcar #'(lambda (k) (assoc k ps)) as)))
      (if (some #'(lambda (kp) (not kp)) kps)
          nil
          (apply (numeric-* n) (mapcar #'cdr kps)))))

(defun label-prob (l ps)
  (union-prob (remove nil (mapcar #'(lambda (e) (env-prob e ps)) l))))

(defun symbolic-label-prob (l ps)
  (symbolic-union-prob (remove nil (mapcar #'(lambda (e) (symbolic-env-prob e ps)) l))))

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
(defun union-prob (vs)
  (union-prob-iter vs 1 (length vs)))

(defun union-prob-iter (vs k n)
  (let ((r (choose vs k)))
    (if (null r)
        0
        (- (apply #'+ (mapcar #'(lambda (x) (apply #'* x)) r))
           (union-prob-iter vs (+ k 1) n)))))

(defun symbolic-union-prob (vs)
  (symbolic-union-prob-iter vs 1 (length vs)))

(defun symbolic-union-prob-iter (vs k n)
  (let ((r (choose vs k)))
    (if (null r)
        0
        (symbolic--
         (apply #'symbolic-+ (mapcar #'(lambda (x) (apply #'symbolic-* x)) r))
         (symbolic-union-prob-iter vs (+ k 1) n)))))

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
  (label-prob (tms-node-label n) ps))

(defun symbolic-node-prob (n ps)
  (symbolic-label-prob (tms-node-label n) ps))

(defun numeric-node-prob (n x ps)
  (numeric-label-prob n (tms-node-label x) ps))

(defun prob-node (node ps &optional (stream t) (prefix ""))
  (format stream "~%<~A~A," prefix (tms-node-datum node))
  (format stream "~2$" (node-prob node ps))
  (format stream ">"))

(defun prob-nodes (atms ps &optional (stream t))
  (dolist (n (reverse (atms-nodes atms))) (prob-node n ps stream)))

(defun why-prob-node (node ps &optional (stream t) (prefix ""))
  (format stream "~%<~A~A," prefix (tms-node-datum node))
  (format stream "~2$:{" (node-prob node ps))
  (dolist (e (tms-node-label node))
    (let ((pe (env-prob e ps)))
      (when pe (format stream "~2$:" pe))
      (env-string e stream)))
  (format stream "}>"))

(defun why-prob-nodes (atms ps &optional (stream t))
  (dolist (n (reverse (atms-nodes atms))) (why-prob-node n ps stream)))

(defun symbolic-prob-node (node ps &optional (stream t) (prefix ""))
  (format stream "~%<~A~A," prefix (tms-node-datum node))
  (format stream "~2$" (symbolic-node-prob node ps))
  (format stream ">"))

(defun symbolic-prob-nodes (atms ps &optional (stream t))
  (dolist (n (reverse (atms-nodes atms))) (symbolic-prob-node n ps stream)))

(defun symbolic-why-prob-node (node ps &optional (stream t) (prefix ""))
  (format stream "~%<~A~A," prefix (tms-node-datum node))
  (format stream "~2$:{" (symbolic-node-prob node ps))
  (dolist (e (tms-node-label node))
    (let ((pe (symbolic-env-prob e ps)))
      (when pe (format stream "~2$:" pe))
      (env-string e stream)))
  (format stream "}>"))

(defun symbolic-why-prob-nodes (atms ps &optional (stream t))
  (dolist (n (reverse (atms-nodes atms))) (symbolic-why-prob-node n ps stream)))

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
