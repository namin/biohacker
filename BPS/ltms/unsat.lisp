(in-package :cl-user)

;; Shows that the contradiction nogood set corresponds to an unsat core.

(defun test-unsat ()
  (setq *ltms* (create-ltms "Example"))
  (tms-create-node *ltms* "a" :ASSUMPTIONP t)
  (tms-create-node *ltms* "b" :ASSUMPTIONP t)
  (tms-create-node *ltms* "c" :ASSUMPTIONP t)
  (enable-assumption (find-node *ltms* "a") :TRUE)
  (enable-assumption (find-node *ltms* "b") :TRUE)
  (enable-assumption (find-node *ltms* "c") :FALSE)
  (compile-formula *ltms* `(:IMPLIES (:OR "a" "b") "c")))

(test-unsat)

;; In slime, type :, then (tms-answer ...).
