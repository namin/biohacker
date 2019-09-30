(bps-load-file (make-bps-path "ltms") "ltre" :action :compile)
(compile-ltre)
(bps-load-file (make-bps-path "ltms") "ltms" :action :compile)
(bps-load-file (make-bps-path "ltms") "sudoku" :action :compile)
