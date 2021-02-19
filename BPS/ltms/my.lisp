(bps-load-file (make-bps-path "ltms") "ltre" :action :compile)
(compile-ltre)
(bps-load-file (make-bps-path "ltms") "ltms" :action :compile)

(bps-load-file (make-bps-path "ltms") "cltms" :action :compile)

(bps-load-file (make-bps-path "ltms") "ltms-ex" :action :compile)
(test-explain)
(run-tests)

(bps-load-file (make-bps-path "ltms") "counterfactual" :action :compile)

;; pages 292-293 of BPS
(setq *ltms* (create-ltms "Simple"))
(setq x (tms-create-node *ltms* "x" :ASSUMPTIONP t) y (tms-create-node *ltms* "y") z (tms-create-node *ltms* "z") r (tms-create-node *ltms* "r"))
(add-formula *ltms* `(:OR ,x ,y))
(add-formula *ltms* `(:OR (:NOT ,y) ,z))
(add-formula *ltms* `(:OR (:NOT ,z) ,r))
(enable-assumption x :FALSE)
(explain-node r)

(bps-load-file (make-bps-path "ltms") "dds" :action :compile)
(Test-DD-search)

(bps-load-file (make-bps-path "ltms") "sudoku" :action :compile)
(solve-sudoku *easy-puzzle* :debugging t)
