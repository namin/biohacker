(require :asdf)
;; in ~/common-lisp
;; clone the following repositories
;; https://github.com/melisgl/named-readtables.git
;; https://github.com/trivial-gray-streams/trivial-gray-streams.git
;; https://github.com/GrammaTech/cl-smt-lib.git
(asdf:make 'named-readtables)
(asdf:make 'trivial-gray-streams)
(asdf:make 'cl-smt-lib)
