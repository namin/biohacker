;; -*- Mode: Lisp; -*-

;;;; Portable listing program for Common Lisp
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;; Parameters involving output device
(defvar *mode* :LPT) ;; Options are :LPT, :EPSON, :LATEX, :BOOK
(defvar *page-width* 80.)
(defvar *page-length* 60.)
(defvar *left-margin* 5.)
(defvar *right-margin* 5.)
(defvar *left-header* "")
(defvar *Italics-start* "")
(defvar *Italics-end* "")
(defvar *lst-file-suffix* "lpt")

;; parameters involving code
(defvar *line-length* 70.) ;; Maximum line of code allowed.
(defvar *lines-per-page* 55.) ;; Number of code lines per page

(setq *lst-latex-file-header*
  '("~%~A" "\\batchmode"
"~%~A" "\\documentstyle[12pt,alltt]{article}"
"~%~A" "\\pagestyle{myheadings}"
"~%~A" "\\makeatletter"
"~%~A" "\\newcommand{\\singlespacing}"
"~A" "{\\let\\CS=\\@currsize\\renewcommand"
"~A" "{\\baselinestretch}{0.5}\\small\\CS}"
"~%~A" "\\newcommand{\\normalspacing}{\\singlespacing}"
"~%~A" "\\makeatother"
"~%~A" "\\textheight 720pt"
"~%~A" "\\textwidth 500pt"
"~%~A" "\\topmargin -0.5in"
"~%~A" "\\oddsidemargin -0.5in"
"~%~A" "\\evensidemargin -0.5in"
"~%~A" "\\begin{document}"
"~%~A" "\\singlespacing"
"~%~A" "\\begin{alltt}"
"~%~A" ""))

;;; First pass -- no indexing information.

(defun lst (fname &key (mode :LPT)
		  (output nil)
		  (header fname))
  (setq *left-header* header
	*page-num* 1
	*new-page?* nil
	*line-num* 0
	*over-length* 0)
  (set-lpr-mode mode) ;; changes other globals as needed
  (unless output 
    (setq output (make-lpt-output-file fname)))
  (with-open-file (fin fname :DIRECTION :INPUT)
   (with-open-file (fout output :DIRECTION :OUTPUT
			 :IF-DOES-NOT-EXIST :create
			 :IF-EXISTS :SUPERSEDE)
     (output-lpr-header fout)
     (print-page-header fout)
     (do ((line (read-line fin nil 'Silly-eof-marker)
		(read-line fin nil 'Silly-eof-marker)))
	 ((eq line 'silly-eof-marker)
	  (finish-off-file fout))
       (process-lpr-line line fout)))))

(defun output-lpr-header (fout)
 (case *mode*
   (:LPT nil) ;; Do nothing
   (:EPSON (format fout "~AG~AE" #\Esc #\Esc)) ;; Emph & Dbl
   (:LATEX (do ((strings *lst-latex-file-header*
			 (cddr strings)))
	       ((null strings))
	       (format fout (car strings) (cadr strings))))
   (:BOOK (format fout "~%\\begin{BPSCode}~%"))))

(defun make-lpt-output-file (fname)
  (let ((path (pathname-directory fname))
	(name (pathname-name fname)))
    (concatenate 'string "/"
		 (reduce #'(lambda (x y) (concatenate 'string
						      x "/" y))
			 (cdr path));; Patch for lucid on RS/6000
			 "/" name "." *lst-file-suffix*)))

;;;; Setting up consequences of printer mode

(defun set-lpr-mode (mode)
  (setq *mode* mode)
  (case mode
    (:LPT (setq *page-width* 80.
		*page-length* 60.
		*left-margin* 5.
		*left-margin-string* "     "
		*right-margin* 5.
		*lst-file-suffix* "lpt"))
    (:EPSON (setq *page-width* 80.
		  *page-length* 60.
		  *left-margin* 5.
		  *left-margin-string* "     "
		  *right-margin* 5.
		  *lst-file-suffix* "prn"
		  *Italics-Start* (format nil "~A4" #\Esc)
		  *Italics-End* (format nil "~A5" #\Esc)))
    ((:LATEX :BOOK)
     (setq *page-width* 80.
	   *page-length* 60.
	   *left-margin* 5.
	   *left-margin-string* ""
	   *right-margin* 5.
	   *lst-file-suffix* "tex"
	   *Italics-Start* "{\\em "
	   *Italics-End* "}"))))

(defun finish-off-file (fout) 
  ;; Eventually, print table of contents and index.
  (when (> *over-length* 0)
	(format t "~% Page ~D is ~D lines too long."
		*page-num* *over-length*))
  (case *Mode*
    (:LPT fout)
    (:EPSON (format fout "~%~AF~AH" #\Esc #\Esc)) ;; turn off Emp/Dbl
    (:LATEX (format fout "~%\\end{alltt}~%\\end{document}"))
    (:BOOK (format fout "~%\\end{BPSCode}"))))

;;;; Processing a line

;; Number of lines on current page.
(defvar *line-num* 0)
;; Number of pages in this listing.
;;  (This isn't the same as page number in book mode!)
(defvar *page-num* 0)
;; Indicates new page has just been generated.
(defvar *new-page?* t)
;; Keeps track of how many lines over one is.
(defvar *over-length* 0)

(defun process-lpr-line (line fout)
  (cond ((new-page-indicated? line)
	 (unless *new-page?*
	   (start-new-page fout)))
	(t (if *new-page?*
	       (start-new-page fout))
	   (incf *line-num*)
	   (when (> *line-num* *page-length*) (incf *over-length*))
	   (when (> (length line) *line-length*)
	     (format t "~% Line ~D on Page ~D is too long by ~D chars"
		     *line-num* *page-num* (- (length line) *line-length*)))
	   (if (or (eq *mode* :LATEX)
		   (eq *mode* :BOOK)) (setq line (hide-latex-schars line)))
	   (setq line (italicize-line-maybe line))
	   (output-lpr-line line fout))))

(defun new-page-indicated? (line) (find #\Page line))

(defun output-lpr-line (line fout)
  (format fout "~%~A~3D ~A" *left-margin-string* *line-num* line))

(defun start-new-page (fout)
  (when (> *over-length* 0)
    (format t "~% Page ~D is ~D lines too long." *page-num* *over-length*))
  (setq *new-page?* nil *line-num* 0 *over-length* 0)
  (incf *page-num*)
  (print-new-page fout)
  (print-page-header fout))

;;;; Starting new pages

(defun print-new-page (fout)
  (case *mode* 
    (:LPT (format fout "~A" #\Page))
    (:EPSON (format fout "~A" #\Page))
    ((:LATEX :BOOK) (format fout "~%\\newpage~%"))))

(defun print-page-header (fout)
  (case *mode*
    (:LPT (format fout "~A, page ~D~%~%"
		  *left-header* *page-num*))
    (:EPSON (format fout "~A, page ~D~%~%"
		    *left-header* *page-num*))
    ((:LATEX :BOOK) (format fout "~A, page ~D~%~%"
			    *left-header* *page-num*))))

;;;; Italicizing comments

(defun italicize-line-maybe (line) ;; A string
  ;; Search for comment marker.  If found,
  ;; wrap appropriate stuff around it to make it come
  ;; out in italics for that mode.
  (case *mode*
    (:LPT line) ;; Do nothing
    ((:EPSON :LATEX :BOOK)
     (let ((start (find-comment-start line)))
       (cond (start
	      (concatenate 'String 
			   (subseq line 0 start)
			   *Italics-start*
			   (subseq line start)
			   *Italics-end*))
	     (t line))))))

(defun find-comment-start (line) (position #\; line))

(defvar *tab-spaces* "        ")

(defvar *latex-substitutions* 
  `((#\Tab . ,*tab-spaces*)
    ;; The order of these is important!
    (#\\ . "\\(\\backslash\\)")
    (#\< . "\\(<\\)")
    (#\> . "\\(>\\)")
    (#\# . "\\#")
    (#\& . "\\&")
;;    (#\$ . "\\$") Assume not in source.  O.W., replace backslash
;; stuff by substring replacement.
    (#\{ . "\\{")
    (#\} . "\\}")
    (#\% . "\\%")))

(defun hide-latex-schars (line)
  ;; Puts backslashes in front of characters latex
  ;; treats as special.  Also removed tabs.
  (dolist (sub *latex-substitutions* line)
	  (setq line (replace-char-by-string (car sub) (cdr sub) line))))

(defun replace-char-by-string (char sub str)
  (do ((pos (position char str))
       (inc (length sub)))
      ((null pos) str)
      (setq str (concatenate 'string (subseq str 0 pos)
			     sub
			     (subseq str (1+ pos))))
      (setq pos (position char str :START (+ pos inc)))))

;;;; Useful variants

(defun lst-directory (path &key (comment "")
			   (mode :LPT))
  (dolist (file (directory (concatenate 'string
					path "*.lisp")))
    (format t "~%Lst'ing ~A.." (pathname-name file))
    (lst file :MODE mode
	 :HEADER (concatenate 'string (pathname-name file) comment))))

(defun lst-files (file-list path &key (mode :LPT) (comment ""))
  (dolist (file file-list (format t "~% Done."))
    (format t "~%Lst'ing ~A.." file)
    (lst (concatenate 'string path file ".lisp")
	 :MODE mode
	 :HEADER (concatenate 'string file comment))))
