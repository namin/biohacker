(bps-load-file (make-bps-path "relax") "waltzer" :action :compile)

(bps-load-file (make-bps-path "relax") "sudoku" :action :compile)

(bps-load-file (make-bps-path "relax") "scene" :action :compile)
(analyze-scene *scene-file* *jcatalog-file*)
(show-scene)