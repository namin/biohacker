;;;; -*- Mode: LISP; Syntax: Common-Lisp; Base: 10;                             -*-
;;;; ------------------------------------------------------------------------------
;;;; File name: marx.lsp
;;;;    System: LTRE
;;;;   Version: 1.0
;;;;    Author: Kenneth D. Forbus
;;;;   Created: Apr 18, 1996
;;;;  Modified: forbus on Thurs Apr 18 10:04:00 1996
;;;;   Purpose: Figuring out which Marx brother was which
;;;; ------------------------------------------------------------------------------

(in-package :COMMON-LISP-USER)

;; This puzzle is a simple example of a constraint satisfaction problem,
;; which can easily be solved via dependency directed search.  Here the
;; problem is figuring out which brothers have which attributes, which 
;; mathematically is equivalent to finding bindings for variables over a discrete 
;; domain.  The LTRE allows us to express these relationships very naturally,
;; using higher-order relations to concisely describe the facts at hand.

(defparameter *attributes* 
   '(PLAYS-PIANO PLAYS-HARP ;; musical talents
     SMOOTH-TALKER LIKES-GAMBLING LIKES-ANIMALS)) ;; interests

(defparameter *objects* '(GROUCHO HARPO CHICO)) ;; Sorry, Zeppo.

(defparameter *constraint-file* "marxdata")

(defun marx-brothers ()
   (solve-attribution-problem *attributes* *objects* *constraint-file*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; General purpose attribute solver

(defun make-attribute-choice-sets (attributes objects)
   ;; Each attribute is assumed to apply to exactly one of the objects.
   (mapcar #'(lambda (attribute)
                (mapcar #'(lambda (object)
                             (list attribute object)) objects))
      attributes))

(defun solve-attribution-problem (attributes objects constraint-file)
   (in-ltre (create-ltre "Attribution Problem Scratchpad"))
   (bps-load-file (make-bps-path "ltms") constraint-file)
   (DD-Search (make-attribute-choice-sets attributes objects)
      `(show-attribute-solution ',attributes)))

(defun show-attribute-solution (attributes)
   (format t "~%Solution:")
   (dolist (attribute attributes)
      (dolist (match (fetch `(,attribute ?object)))
         (when (true? match)
            (format t "~%  ~A" match)))))

   