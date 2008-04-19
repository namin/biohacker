;; -*- Mode: Lisp; Package: common-lisp-user; -*-

;;;; Acceptance tests for LTRE
;; Last edited 4/27/95, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

(defun test-ltre ()
  (in-ltre (create-ltre "Debugging LTRE"))
  (format t "~%Testing database/LTMS link...")
  (test-datums)
  (format t "~%Testing LTMS...")
  (test-clauses)
  (format t "~%Testing Rule system...")
  (test-rules))

(defun test-datums ()
  (assert! 'foo 'testing)
  (unless (true? 'foo) (error "Fact installation glitch"))
  (assert! '(:NOT bar) 'testing)
  (unless (false? 'bar) (error "Negation glitch"))
  :OKAY)

(defun test-clauses ()
  (assert! '(:OR a b) 'case-split)
  (assert! '(:IMPLIES a c) 'why-not?)
  (assume! '(:IMPLIES c d) 'what-the-heck)
  (assume! '(:NOT b) 'for-fun)
  (unless (true? 'd) (error "Propagation glitch"))
  (retract! '(:NOT b) 'for-fun)
  (unless (unknown? 'd) (error "Retraction glitch"))
  (assume! '(:NOT b) 'for-fun)
  (unless (true? 'd) (error "Unouting glitch"))
  (retract! '(:IMPLIES c d) 'what-the-heck)
  (unless (unknown? 'd) (error "Retraction glitch 2"))
  (assume!'(:IMPLIES c d) 'what-the-heck)
  (unless (true? 'd) (error "Unouting glitch 2"))
  :OKAY)

(defun test-rules ()
  (eval `(rule ((:TRUE (foo ?x) :VAR ?f1)
		(:TRUE (bar ?y) :VAR ?f2))
	       (rassert! (:IMPLIES (:AND ?f1 ?f2) (mumble ?x ?y)) 'hack)))
    (eval `(rule ((:INTERN (foo ?x) :VAR ?f1)
		  (:INTERN (bar ?y) :VAR ?f2))
	       (rassert! (:IMPLIES (:AND ?f1 ?f2) (grumble ?x ?y)) 'hack)))
  (referent '(foo 1) t)
  (referent '(bar 1) t)
  (run-rules)
  (unless (referent '(grumble 1 1) nil) (error "Intern triggering failure"))
  (when (referent '(mumble 1 1) nil) (error "Premature triggering"))
  (assume! '(foo 1) 'why-not?)
  (assume! '(:not (bar 1)) 'monkeywrench)
  (run-rules)
  (when (true? '(mumble 1 1)) (error "Badly conditioned triggering"))
  (retract! '(:not (bar 1)) 'tweak)
  (unless (false? '(bar 1)) (error "Retraction with wrong informant"))
  (retract! '(:not (bar 1)) 'monkeywrench)
  (run-rules)
  (when (true? '(mumble 1 1)) (error "Badly conditioned triggering - 2"))
  (assume! '(bar 1) 'why)
  (run-rules)
  (unless (true? '(mumble 1 1)) (error "Badly conditioned triggering - 2"))
  (assume! '(foo 2) 'go-for-it)
  (run-rules)
  (unless (true? '(mumble 2 1)) (error "Rule chaining failure"))
  (assume! '(bar 2) 'alternate)
  (run-rules)
  (unless (true? '(mumble 1 2)) (error "Subrule spawning failure"))
  (unless (true? '(mumble 2 2)) (error "Subrule spawning failure - 2"))
  :OKAY)
