;; -*- Mode: Lisp; -*-

;;;; Test code for ATRE Blocksworld system
;; Last edited: 1/29/93, by KDF

;; Copyright (c) 1990-1992 Kenneth D. Forbus,  Northwestern
;; University, and Johan de Kleer, Xerox Corporation.  
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defvar *blocks-file*
  #+ILS  "/u/bps/code/atms/blocks"
  #+PARC "virgo:/virgo/dekleer/bps/code/atms/blocks"
  #+MCL "Macintosh HD:BPS:atms:blocks")

(defun build-blocks-problem (title blocks-list
				   &optional (debugging nil)
				   &aux plnpr)
  (setq plnpr
	(create-planning-problem
	 title (make-blocks-basis-set blocks-list)))
  (in-plnpr plnpr)
  (set-debug-plnpr debugging)
  (with-atre (plnpr-atre plnpr)
   (load *blocks-file*) ;; Load basic definitions
   (dolist (block blocks-list)
	   (assert! `(block ,block) 'Definition))
   (run-rules)
   (setup-choice-sets plnpr))
  plnpr)

(defun make-blocks-basis-set (blocks &aux basis)
  (dolist (block blocks)
	  ;; what the block can be on.
      (push `((Holding ,block) (On ,block Table)
	      ,@ (mapcar #'(lambda (other)
			     `(On ,block ,other))
			 (remove block blocks)))
	    basis)
      ;;; What can be on the block
      (push `((Holding ,block) (Clear ,block)
	      ,@ (mapcar #'(lambda (other)
			     `(ON ,other ,block))
			 (remove block blocks)))
	    basis))
    (cons `((HAND-EMPTY)
	    ,@ (mapcar #'(lambda (block)
			   `(HOLDING ,block)) blocks))
	  basis))
