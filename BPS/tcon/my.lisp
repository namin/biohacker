(bps-load-file (make-bps-path "tcon") "tcon" :action :compile)

(bps-load-file (make-bps-path "tcon") "sudoku" :action :compile)
(solve-sudoku *easy-puzzle*)
