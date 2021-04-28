(require :asdf)
;; in ~/common-lisp
;; clone the following repositories
;; https://github.com/melisgl/named-readtables.git
;; https://github.com/trivial-gray-streams/trivial-gray-streams.git
;; and instead of https://github.com/GrammaTech/cl-smt-lib clone
;; https://github.com/namin/cl-smt-lib.git
;; and checkout the my branch
(asdf:make 'named-readtables)
(asdf:make 'trivial-gray-streams)
(asdf:make 'cl-smt-lib)
