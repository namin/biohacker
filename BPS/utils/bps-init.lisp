*;;;; -*-  Mode: LISP; Syntax: Common-Lisp; Base: 10                          -*-
;;;; ---------------------------------------------------------------------------
;;;; File name: bps-init.lsp
;;;;    System: Building Problem Solvers
;;;;   Version: 1.0
;;;;    Author: John Everett
;;;;   Created: Mar 04, 1995
;;;;  Modified: Wednesday, January 3, 2001 at 15:28:51 by forbus
;;;;   Purpose: To facilitate using the Building Problem Solvers code
;;;; ---------------------------------------------------------------------------

(in-package :cl-user)

;;; SETUP A BPS PACKAGE THAT CONTAINS UTILITIES SHARED BY ALL BPS SYSTEMS
;;; ----------------------------------------------------------------------------
;;; The BPS package provides a little bundle of utilities that all BPS systems
;;; should have access to, so whenever you create a new package for a BPS
;;; system, be sure that it uses this package.  The Common-LISP USER package
;;; uses this package so that its small suite of exported functions (load-file,
;;; load-files, and make-BPS-path) are transparently available in CL-USER (the
;;; default package on startup).

;(unless (find-package :bps)
;(make-package :bps :use '(:common-lisp))
;(use-package (find-package :bps) (find-package :cl-user)))
;(in-package :bps)

;;; EXPORT bps SYMBOLS
;;; ----------------------------------------------------------------------------
;;; These are the public bps symbols that all bps systems should have access to.

(export '(bps-load-file
           bps-load-files
           make-bps-path
           make-bps-source-file-name
           bps-menu
           *bps-use-packages*))


;;; GLOBAL VARIABLES
;;; ----------------------------------------------------------------------------
;;; These global variables are available in the common-lisp-user package.
;;; Rebinding them will alter the behavior the loading functions.  For example,
;;; rebinding the *bps-path* variable will cause the loading functions to look
;;; elsewhere for bps files.

(defvar *bps-path*     nil "Path stub to which all systems add their pathnames")
(defvar *path-sepchar* nil "Path separation character (system-dependent)")
(defvar *src-ext*      nil "Source extension for files")
(defvar *bin-ext*      nil "Binary or fasload extension for files")
(defvar *bps-use-packages* nil "Default use packages for BPS systems.")

;;; USER-SPECIFIC CONFIGURATION SECTION
;;; ============================================================================
;;; You must ensure that the following numbered items are set correctly for your
;;; particular hardware and software.


;1. SETTING THE LOCAL BPS PATH STUB --------------------------------------------
;;; This path stub must include a terminal path separation character.

(setq *bps-path* *bps-home*)


;;; END OF USER-SPECIFIC CONFIGURATION
;;; ============================================================================
      

;;; HARDWARE-SPECIFIC SETTINGS 
;;; ----------------------------------------------------------------------------
;;; You should not change the following settings, except to add new cases.

(setf *path-sepchar*
      "/"
      )

(setf *src-ext*
      ".lisp"
      )

(setf *bin-ext*
      ".fasl"
      )

(setf *bps-use-packages* nil)

;;; THE BPS LOADING FACILITY
;;; ----------------------------------------------------------------------------
;;; See bps-system-spec.text (located in *bps-path*) for a detailed discussion
;;; of this loading facility.  The following are notes on specific aspects of
;;; the code:
;;; 1. :COMPILE-IF-NEWER results in an error (LUCID/RS6000) if the file
;;; contains, in this order, (1) a macro, (2) defun forms defining functions
;;; called by the macro, and (3) a top-level call to the macro, apparently
;;; because the compilation process does not incrementally load the forms of the
;;; file, so the first defun is undefined at the time that the compiler hits the
;;; top-level call.  A solution to this would be to have the :COMPILE-IF-NEWER
;;; first load the source, then compile and load the binary, but this would be
;;; slower.  Another solution would be to ensure that toplevel calls occur only
;;; in their own files, separate from their defining functions.  As of 2/25/95,
;;; going with this second "solution," but will implement the first if demand
;;; warrants.

(defmacro with-compiler-chatter (messages? &rest body)
  "Implementation-specific means of controlling compiler messaging"
  (cond ((member :MCL *features*)
         `(let (#+MCL (ccl::*suppress-compiler-warnings* ,messages?))
            ,@body))
        ((member :LUCID *features*)
         `(cl-user::with-compiler-options (:warnings ,messages?
                                                     :messages ,messages?)
                                          ,@body))
        (t ;;Add branches for other implementation defintions
	  `(progn ,@body))))

(defun bps-load-files (path file-list
                            &key (action :load-source) (verbose t)
			(src-ext *src-ext*) (bin-ext *bin-ext*))
  (unless verbose
    (format t "Suppressing messages during this load of files from~% ~A~%~
               To see messages, call bps-load-files with the keyword :verbose ~
               set to T~%~%"
	    path))
  (ecase action
	 ((:compile-if-newer :source-if-newer :load-source :load-binary)
	  (dolist (file file-list)
	    (bps-load-file path file
		       :action action
		       :verbose verbose
		       :src-ext src-ext
		       :bin-ext bin-ext)))
	 (:compile
	  (bps-load-files path file-list
		      :action :load-source
		      :verbose verbose
		      :src-ext src-ext
		      :bin-ext bin-ext)
	  (dolist (file file-list)
	    (bps-load-file path file
		       :action :compile
		       :verbose verbose
		       :src-ext src-ext
		       :bin-ext bin-ext)))))

(defun bps-load-file (path file
		       &key (action :load-source) (verbose t)
		       (src-ext *src-ext*) (bin-ext *bin-ext*))
  (ecase action
	 (:compile-if-newer
	  (let* ((src (make-full-file-spec path file src-ext))
		 (bin (make-full-file-spec path file bin-ext))
		 (src-exists? (probe-file src))
		 (bin-exists? (probe-file bin)))
	    (cond ((and src-exists? bin-exists?)
		   (cond ((>= (file-write-date bin) (file-write-date src))
			  (load bin :verbose verbose))
			 (t (with-compiler-chatter verbose
                              (compile-file src) )
                            (load bin :verbose verbose))))
		  (bin-exists? (load bin :verbose verbose))
		  (src-exists?
		   (bps-load-file path file
				  :action :load-source
				  :verbose verbose
				  :src-ext src-ext
				  :bin-ext bin-ext)
		   (with-compiler-chatter verbose
		     (compile-file src))
                   (load bin :verbose verbose))
		  (t (error "~%Can't find ~A or ~A~%" src bin)))))
	 (:source-if-newer
	  (let* ((src (make-full-file-spec path file src-ext))
		 (bin (make-full-file-spec path file bin-ext))
		 (src-exists? (probe-file src))
		 (bin-exists? (probe-file bin)))
	    (cond ((and src-exists? bin-exists?)
		   (cond ((>= (file-write-date bin) (file-write-date src))
			  (load bin :verbose verbose))
			 (t (load src :verbose verbose))))
		  (bin-exists? (load bin :verbose verbose))
		  (src-exists? (load src :verbose verbose))
		  (t (error "~%Can't find ~A or ~A~%" src bin)))))
	 (:compile
	  (bps-load-file path file
			 :action :load-source
			 :verbose verbose
			 :src-ext src-ext
			 :bin-ext bin-ext)
	  (with-compiler-chatter verbose
            (compile-file (make-full-file-spec path file src-ext))))
	 (:load-source
	  (load (make-full-file-spec path file src-ext)
		:verbose verbose))
	 (:load-binary
	  (load (make-full-file-spec path file bin-ext)
		:verbose verbose))))

(defun make-full-file-spec (path file ext)
  "Ensures that a path separator character appears between path and file"
  (cond ((= (position *path-sepchar* (reverse path) :test #'string=) 0)
	 (concatenate 'string path file ext))
	(t (concatenate 'string path *path-sepchar* file ext))))

(defun make-bps-path (&rest strings)
  (concatenate 'string *bps-path*
	       (reduce #'(lambda (str1 str2)
			   (concatenate 'string str1 *path-sepchar* str2))
		       strings)
	       *path-sepchar*))

(defun make-bps-source-file-name (path name)
   (concatenate 'string path name *src-ext*))


;;; THE TOP LEVEL OF THE TEXT MENU LOADING FACILITY
;;; ----------------------------------------------------------------------------
;;; You can, of course, simply call the loading functions defined in each system
;;; top-level file (e.g., "load-tre" is defined in tre.lisp).


(defun bps-menu (&aux choice)
  (format t "~%~%Enter the number of the system you would like to load~%~
             from the list below or type q to quit. You can recall~%~
             this menu at any time by typing (bps-menu)~%~%")
  (do ((done? nil))
      (done? (values))
    (display-menu)
    (multiple-value-setq (choice done?)
      (get-user-choice))
    (cond ((eq choice :quit)
	   (setq done? t)
           (values))
          (done?
           ;; Set up default for other files
           (setq *default-pathname-defaults* 
             (parse-namestring *bps-path*))
           ;; Load the desired system
	   (load-bps-system-choice choice))
	  (t (terpri)))))
             
  
(defun load-bps-system-choice (choice &aux load-action)
  (setf load-action (if (y-or-n-p "Compile this system?") 
                       :compile 
                       :load-source))
   (ecase (parse-integer choice)
     (1
      (load-bps-system "cps" "cps" "load-cps" load-action))
     (2
      (load-bps-system "tre" "tre" "load-tre" load-action))
     (3
      (load-bps-system "ftre" "ftre" "load-ftre" load-action))
     (4
      (load-bps-system "jtms" "jtre" "load-jtre" load-action))
     (5
      (load-bps-system "jtms" "jtre" "load-jsaint" load-action))
     (6
      (load-bps-system "ltms" "ltre" "load-ltre" load-action))
     (7
      (load-bps-system "tgizmo" "tgizmo" "load-tgizmo" load-action))
     (8
      (load-bps-system "atms" "atre" "load-atre" load-action))
     (9
      (load-bps-system "atms" "atre" "load-atre-planner" load-action))
     (10
      (load-bps-system "tcon" "tcon" "load-tcon" load-action))
     (11
      (load-bps-system "gde" "atcon" "load-atcon" load-action))
     (12
      (load-bps-system "gde" "gde" "load-gde" load-action))
     (13
      (load-bps-system "relax" "relax" "load-waltzer" load-action))
     ))



;;; BPS SYSTEMS MENU LOADING FACILITY BEHIND THE SCENES FUNCTIONS
;;; ----------------------------------------------------------------------------
;;; Presents a simple text menu for choosing a BPS system to load.  This file
;;; requires functions defined in bps-init.lisp.

(defparameter *bps-menu-alist* nil "Alist containing menu info")

(setf *bps-menu-alist*
      '((1 . "Classical Problem Solver (CPS)")
        (2 . "Tiny Rule Engine (TRE)")
        (3 . "Fast Tiny Rule Engine (FTRE)")
        (4 . "Justification-based TMS/TRE (JTMS/JTRE)")
        (5 . "JSaint System")
        (6 . "Logic-based TMS/TRE (LTMS/LTRE)")
        (7 . "Qualitative Physics Reasoner (TGizmo)")
        (8 . "Assumption-based TMS/TRE (ATMS/ATRE)")
	(9 . "ATRE-based Planner")
	(10 . "Tiny Constraint Language (TCON)")
	(11 . "ATRE-based Tiny Constraint Language (ATCON)")
        (12 . "General Diagnostic Engine (GDE)")
        (13 . "Symbolic Relaxation Engine (Waltzer)")))

(defun display-menu (&optional (strm *standard-output*))
  (dolist (indexed-item *bps-menu-alist*)
    (format strm "~3@A. ~A~%" (car indexed-item) (cdr indexed-item)))
  (format strm "     Type Q to quit from this menu system~%")
  (format strm "~%>> "))
    
(defun get-user-choice ()
  "Return choice and truth value indicating choice validity"
  ;;#+LUCID (read-line) ;;bc otherwise we get unwanted return
  (let ((input (read)))
    (if (numberp input) (setq input (write-to-string input)))
    (cond ((member input '("quit" "exit" "q" ":q" "bye") :test #'string-equal)
           (values :quit nil))
          ((and (member input (get-valid-inputs)
                        :test #'string-equal)
                (y-or-n-p "Load the ~A? " 
                          (cdr (assoc  (parse-integer input) 
                                       *bps-menu-alist*))))
           (values input t))
          (t (values input nil)))))

(defun get-valid-inputs ()
  (mapcar #'(lambda (menu-item)
              (write-to-string (car menu-item)))
          *bps-menu-alist*))

(defun load-bps-system (relative-path defsys-file load-fn &optional (action :load-source))
   "Load the given BPS system. Load function should be given as a string."
   (bps-load-file (make-bps-path relative-path) defsys-file :action :load-source)
   ;; Funcall function as string in order to avoid later package problems.
   (funcall (intern load-fn :cl-user) :action action))

;;; Displaying menu on startup doesn't work on ACLPC.


;;; END OF CODE-----------------------------------------------------------------

