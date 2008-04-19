;; -*- Mode: Lisp; -*-

;;;; SCENE -- Scene analysis by line labelling
;;; Last edited 1/29/93, by KDF

;; Copyright (c) 1988-1992 Kenneth D. Forbus, Northwestern University,
;;   Johan de Kleer, Xerox Corporation.
;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;; This system uses WALTZER and the line/junction vocabulary from
;; Winston's "Artificial Intelligence", Second Edition, page 56.
;;
;; We represent lines and junctions by cells.  Associated with each
;; junction is a constraint that enforces consistent labellings.


(defvar *scene* nil) ;; Constraint network representing the current scene
(defvar *jlabel-table* nil) ;Alist of (<junction type> . <junction labels>)
(defvar *line-labels* '(+ - < >)) ;simple line labels.

;;;; Interface

;; Some default values
(defvar *scene-file* 
  #+ILS "/u/bps/code/relax/cube"
  #+PARC "virgo:/virgo/dekleer/bps/code/relax/cube"
  #+MCL "Macintosh HD:BPS:relax:cube"
  #+ACLPC "e:\\code\\relax\\cube")

(defvar *jcatalog-file* 
  #+ILS "/u/bps/code/relax/jcatalog.lisp"
  #+PARC "virgo:/virgo/dekleer/bps/code/relax/jcatalog.lisp"
  #+MCL "Macintosh HD:BPS:relax:jcatalog.lisp"
  #+ACLPC "e:\\code\\relax\\jcatalog.lsp")

(defun analyze-scene (scene jcatalog)
  (setq *scene-file* scene)
  (setq *jcatalog-file* jcatalog)
  (read-junction-catalog jcatalog)
  (load scene)
  (interpret-scene))

(defun interpret-scene ()
  (clear-network *scene*)
  (fire-constraints *scene*))

(defun show-scene ()
  (dolist (cell-entry (reverse (network-cells *scene*)))
    (what-is (cdr cell-entry))))

;;;; Defining a junction label catalog.
;;;
;;; Must build the jlabel, and add it to the proper index
;;; under the *jlabel-table*

(defstruct (jlabel
	     (:PRINT-FUNCTION
	       (lambda (jl st ignore) (declare (ignore ignore))
		 (format st "~A-~A" 
			 (jlabel-type jl) (jlabel-name jl)))))
  (name nil)	;name of this label 
  (type nil) ;type of junction it is associated with
  (lines nil))

(defmacro Junction-labelling (type name &rest parts)
  `(let ((junc (make-jlabel
		:NAME ',name
		:TYPE ',type
		:LINES (parse-junction-parts ',parts ',name
					     #'(lambda (name role owner)
						 name))))
	 (entry nil))
     (setq entry (assoc ',type *jlabel-table*))
     (unless entry
       (push (setq entry (cons ',type nil))
	     *jlabel-table*))
     (setf (cdr entry) (cons junc (cdr entry)))
     junc))

(defun read-junction-catalog (file)
  (let ((fpointer (open file))
	(marker (list 'foo)))
    (setq *jlabel-table* nil)
    (do ((form (read fpointer nil marker)
	       (read fpointer nil marker))
	 (counter 0 (1+ counter)))
	((eq form marker)
	 (format t "~%~A read, ~D entries." file counter))
      (eval form))))

;;;; Making the scene (so to speak)
;;; A scene consists of a collection of lines and junctions.
;;; For simplicity we will assume no forward references, i.e.
;;; that a junction will only be specified after the lines it
;;; mentions have been already defines.

(defmacro Scene (name &rest args)
  `(setq *scene* (create-network ,name ,@ args)))

(defmacro Line (name)
  (unless (symbolp name) (error "~%~A bad name for LINE -- must be symbol." name))
  `(build-cell ',name *scene* (copy-list *line-labels*)))

(defmacro Junction (name type &rest parts)
   (unless (symbolp name) (error "~%~A bad name for JUNCTION -- must be symbol." name))
   (unless (assoc type *jlabel-table*)
     (error "~%~A is unknown junction type ~A." name type))
   `(let ((parts (parse-junction-parts ',parts ',name 'fetch-junction-part))
	  (name ',name) (type ',type)
	  (junc nil) (jcon nil))
	    (setq junc (build-cell name *scene* (cdr (assoc type *jlabel-table*))))
;;	    (setf (getf (cell-plist junc) 'junction-type) type)
;;	    (setf (getf (cell-plist junc) 'Parts) parts)
	    (setq jcon (build-constraint (cons name type) *scene* #'update-junction))
	    (setf (constraint-parts jcon) (cons junc parts))
	    (add-constraint-cell junc jcon)
	    (dolist (part-pair parts)
	      (add-constraint-cell (cdr part-pair) jcon))))

(defun parse-junction-parts (part-list owner to-do)
  (cond ((null part-list) nil)
	((not (listp part-list))
	 (error "~%Badly formatted junction part list -- ~A." part-list))
	((and (keywordp (car part-list))
	      (cadr part-list)
	      (symbolp (cadr part-list)))
	 (cons (cons (car part-list)
		     (funcall to-do
			      (cadr part-list) (car part-list) owner))
	      (parse-junction-parts (cddr part-list) owner to-do)))
	(t (error "~%~A incorrectly formed junction part list: ~A"
			  owner part-list))))

(defun fetch-junction-part (part-name role owner)
  (let ((part (lookup-cell part-name *scene*)))
    (unless part
	    (error "~A not found, claimed as ~A of ~A."
		   part-name role owner))
    part))

;;;; Junction updater

(defun update-junction (con)
  (let ((jcell (car (constraint-parts con)))
	(lines (cdr (constraint-parts con))))
    (do ((jlabels (cdr (cell-value jcell)) (cdr jlabels))
	 (jlabel (car (cell-value jcell)) (car jlabels))
	 (possible-line-labels
	   (mapcar #'(lambda (pair)
		       (cons (car pair) nil)) lines)))
	((null jlabel) ;Must propagate ruled out line labels.
	 (dolist (line-label-entry possible-line-labels)
	   (let ((line (cdr (assoc (car line-label-entry) lines)))
		 (possible-labels (cdr line-label-entry)))
	     (dolist (label *line-labels*)
	       (unless (member label possible-labels)
		 (queue-cell line :EXCLUDE label con))))))
      (cond ((check-junction-label jlabel lines) ;exclude this junction label
	     (queue-cell jcell :EXCLUDE jlabel con))
	    (t ;;store line labels associated with this junction
	     ;;as being possible.
	     (dolist (line-entry (jlabel-lines jlabel))
	       (let ((oentry (assoc (car line-entry)
				    possible-line-labels)))
		 (pushnew (cdr line-entry) (cdr oentry)))))))))

(defun check-junction-label (jlabel lines)
  (dolist (line-entry (jlabel-lines jlabel))
    (let ((line (cdr (assoc (car line-entry) lines))))
      (unless (member (cdr line-entry) (cell-value line)) 
	(return t)))))

(defun show-junction (con)
  (format t "~% ~A constrains ~A" (constraint-name con) (cdr (constraint-parts con)))
  (format t "~% Possible labelings:")
  (dolist (jl (cell-value (car (constraint-parts con))))
    (format t "~%    ~A = ~A" jl (jlabel-lines jl))))

(defun show-junctions ()
  (dolist (con-entry (network-constraints *scene*))
    (show-junction (cdr con-entry))))
