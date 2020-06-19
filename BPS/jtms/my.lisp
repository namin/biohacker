(bps-load-file (make-bps-path "jtms") "jtre" :action :compile)
(compile-jtre)
(bps-load-file (make-bps-path "jtms") "jtms" :action :compile)

(bps-load-file (make-bps-path "jtms") "jqueens" :action :compile)
(test-queens 1 8)
