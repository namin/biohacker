(bps-load-file (make-bps-path "ltms") "ltre" :action :compile)
(compile-ltre)
(bps-load-file (make-bps-path "ltms") "ltms" :action :compile)

(bps-load-file (make-bps-path "ltms") "cltms" :action :compile)

(bps-load-file (make-bps-path "ltms") "ltms-ex" :action :compile)
(test-explain)
(run-tests)

(bps-load-file (make-bps-path "ltms") "dds" :action :compile)
(Test-DD-search)

(bps-load-file (make-bps-path "ltms") "sudoku" :action :compile)
(solve-sudoku *easy-puzzle* :debugging t)
