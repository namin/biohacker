;; network-debugger.lisp should
;; * define *nd-path*
;; * load the BPS/utils/init

(defparameter *nd-files*
  '("nd-inter"  ;; Network Debugger Basic Interface
    "nd"        ;; Main Network Debugger
    "utils"     ;; Various utilities
    ))

(defparameter *nd-rules-file* 
  (make-bps-source-file-name *nd-path* "nd-rules"))

(defparameter *nd-extended-rules-file* 
  (make-bps-source-file-name *nd-path* "nd-extended-rules"))

(defparameter *nd-pathway-rules-file* 
  (make-bps-source-file-name *nd-path* "nd-pathway-rules"))

(defun compile-nd ()
  (compile-load-files *nd-files* *nd-path*))

(defun ready-nd ()
  (load (make-bps-source-file-name (make-bps-path "ltms") "ltre"))
  (compile-ltre)
  (compile-nd))

