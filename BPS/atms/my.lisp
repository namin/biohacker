(bps-load-file (make-bps-path "atms") "atre" :action :compile)
(bps-load-file (make-bps-path "atms") "atms" :action :compile)
(compile-atre)
;;(compile-planner)
(bps-load-file (make-bps-path "atms") "sudoku" :action :compile)

