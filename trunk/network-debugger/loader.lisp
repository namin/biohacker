;; network-debugger.lisp should
;; * define *nd-path*
;; * load the BPS/utils/init

(defparameter *nd-files*
  '("nd"        ;; Network Debugger
    ))

(defparameter *nd-rules-file* 
  (make-bps-source-file-name *nd-path* "nd-rules"))

(defun compile-nd ()
  (compile-load-files *nd-files* *nd-path*))

(defun ready-nd ()
  (load (make-bps-source-file-name (make-bps-path "ltms") "ltre"))
  (compile-nd))

