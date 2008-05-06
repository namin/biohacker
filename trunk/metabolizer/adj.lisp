(in-package :ecocyc)
;;;   Copyright (C) 1993-2004 by SRI International.  All rights reserved.

;  This file contains functions that convert the representation of molecules
;  used in the CompoundKB into an adjacency-list representation that is much
;  more convenient to use for certain computations.


; =======================================================  adj-list

; A shorthand for calling make-adj-list

(defun adj-list (compound)
  (make-adj-list
   (get-slot-values compound 'structure-atoms)
   (get-slot-values compound 'structure-bonds)
   (get-slot-values compound 'atom-charges)
   ) )


; =======================================================  make-adj-list

; Creates an adj-list describing a chemical structure.  The list has
; one element per atom in the structure.  The CAR of each element is
; the name of an atom, and the CDR is a bond list; the CAR of each
; element of that bond list is the index in the adj-list of the bonded
; atom, and the CDR is the type of bond.  We use an index origin of 1
; for consistency with structure-atoms.  An atom charge of +2 is encoded
; as a bond of (+ 2)
;
; Example for carbon dioxide:
;
; ((C (3 2) (2 2))
;  (O (1 2))
;  (O (1 2))) 

(defun make-adj-list (atom-list bond-list &optional charge-list)
  (let ((adj-list nil))

    (setq adj-list (list (list (first atom-list))))
    (dolist (atom (cdr atom-list))
      (nconc adj-list (list (list atom))) )


    (dolist (bond bond-list)
      (rplacd (adj-i adj-list (car bond))
	      (cons (list (cadr bond)
			  (caddr bond))
		    (cdr (adj-i adj-list (car bond))))
	      )
      (rplacd (adj-i adj-list (cadr bond))
	      (cons (list (car bond)
			  (caddr bond))
		    (cdr (adj-i adj-list (cadr bond))))
	      )
      )

    (dolist (charge charge-list)
      (rplacd (adj-i adj-list (car charge))
	      (cons (list (if (> (cadr charge) 0)
			      '+
			    '-)
			  (abs (cadr charge)))
		    (cdr (adj-i adj-list (car charge))))))

    adj-list
    ))


;  ==================================================  adj-compute-weight

(defun adj-compute-weight (adjlist)
  (let ((weight 0.0))

    (dolist (adj adjlist)
	    (setq weight (+ weight
			    (or
			     (when (and adj ; kr97-07-28: inserted this test to become NIL-tolerant
					;;kr98-05-20: added this test as well, to
					;; tolerate weird elements
					(coercible-to-frame-p (first adj)) )
			       (gfp:get-slot-value (first adj) 'atomic-weight) )
			     0.0) ))
	    )
    weight
    ))

; =======================================================  compute-formula-from-adj

;; Compute the empirical formula of an adj-list.  This function does not
;; hydrogenate the adj-list, therefore the resulting formula may omit
;; hydrogens if the original adj-list does.

(defun compute-formula-from-adj (adjlist)
  (let ((formula nil)
	atom f)

    (dolist (adj adjlist)
      (setq atom (first adj))
      (if (setq f (assoc atom formula))
	  (rplaca (cdr f)
		  (1+ (second f)))
	  (push (list atom 1)
		formula) )
		  )

    (order-formula formula)
    ) )




(defun entry-adj-list (entry)
  (make-adj-list (get-slot-values entry 'structure-atoms)
		 (get-slot-values entry 'structure-bonds)
		 (get-slot-values entry 'atom-charges))
  )


(defun adj-i (adj-list i)
  (nth (1- i) adj-list)
  )
