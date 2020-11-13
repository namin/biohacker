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

(defun numeric-env-prob (n e ps)
    (let* ((as (env-assumptions e))
           (kps (mapcar #'(lambda (k) (assoc k ps)) as)))
      (if (some #'(lambda (kp) (not kp)) kps)
          nil
          (apply (numeric-* n) (mapcar #'cdr kps)))))

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

(defun numeric-node-prob (n x ps)
  (numeric-label-prob n (tms-node-label x) ps))
