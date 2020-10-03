(setq *pack-ltms* t)
(bps-load-file (make-bps-path "ltms") "ltms" :action :compile)
(bps-load-file (make-bps-path "ltms") "cltms" :action :compile)
(in-package :COMMON-LISP-USER)
(bps-load-file (make-bps-path "atms") "atre" :action :compile)
(bps-load-file (make-bps-path "atms") "atms" :action :compile)
(compile-atre)
;;(compile-planner)

(bps-load-file (make-bps-path "atms") "prob" :action :compile)

(bps-load-file (make-bps-path "atms") "sudoku" :action :compile)
(solve-sudoku *easy-puzzle*)

(bps-load-file (make-bps-path "atms") "atest" :action :compile)
(atms-test1)
(atms-test2)
(step-1)
