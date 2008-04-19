;; -*- Mode: Lisp; -*-

;;;; Tiny Gizmo, a partial implementation of QP theory
;;; Last Edited: 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

;;;; Gizmo was the first implementation of QP theory, which
;;;; evolved from 1982 to 1984.  Like the original, TGizmo
;;;; is based on an LTMS.  However, we've simplified the modeling
;;;; language, and not implemented limit analysis or temporal
;;;; projection here to keep the size of the code small.  
;;;; We show how to use dependency-directed search to interpret  
;;;; measurements within a single qualitative behavior, as per
;;;; Forbus' IJCAI-83 paper.

(in-package :COMMON-LISP-USER)

(defvar *tgizmo-path*
  #+ILS "/u/bps/code/tgizmo/"
  #+PARC "virgo:/virgo/dekleer/bps/code/tgizmo/"
  #+MCL "Macintosh HD:BPS:tgizmo:")

(defvar *tgizmo-files* 
  '("defs"           ; Definitions of internal structs.
    "mlang"          ; Simple modeling language for QP descriptions.
    "psvs"           ; Finding view and process structures.
    "resolve"        ; Influence resolution.
    "ineqs"          ; Inequality reasoning.
    "states"         ; Caching states.
    "mi"             ; Measurement Interpretation system.
    "debug"))        ; Various examples and debugging procedures.

;;; The file laws.lisp contains PDIS rules that enforce logical
;;; constraints of QP theory.  It must be compiled after a tgizmo
;;; has been created.

(defun compile-tgizmo ()
  (compile-load-files *tgizmo-files* *tgizmo-path*)
  (unless (and (boundp '*tgizmo*)
               (not (null *tgizmo*)))
    (create-tgizmo "DUMMY"))
  (compile-load-files '("laws" "tnst") *tgizmo-path*))
