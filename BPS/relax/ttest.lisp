; -*- Mode: Lisp; -*-

;;;; Test cases for tlogic/waltzer, using Allen's time logic.
;; Last edited 1/29/93, by KDF

;;; Copyright (c) 1993, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

(in-package :COMMON-LISP-USER)

(defun test1 (&optional (debug? nil))
  (in-timedb (create-timedb "Test One" *trels-file* debug?))
  (interval a)
  (interval b)
  (interval c)
  (clear-network (timedb-network *timedb*))
  (tassert a b (<))
  (tassert b c (<))
  (tassert a c)
  (what-times))

(defun test2 (&optional (debug? nil))
  (in-timedb (create-timedb "Test Two" *trels-file* debug?))
  (interval a)
  (interval b)
  (interval c)
  (clear-network (timedb-network *timedb*))
  (tassert a b (d))
  (tassert b c (d))
  (tassert a c)
  (what-times))

(defun test3 (&optional (debug? nil))
  (in-timedb (create-timedb "Test Three" *trels-file* debug?))
  (interval a)
  (interval b)
  (interval c)
  (clear-network (timedb-network *timedb*))
  (tassert a b (m))
  (tassert b c (f))
  (tassert a c)
  (what-times))

(defun test4 (&optional (debug? nil))
  (in-timedb (create-timedb "Test Four" *trels-file* debug?))
  (interval a)
  (interval b)
  (interval c)
  (clear-network (timedb-network *timedb*))
  (tassert a b (o))
  (tassert b c (o))
  (tassert a c)
  (what-times))

(defun test5 (&optional (debug? nil))
  (in-timedb (create-timedb "Test Five" *trels-file* debug?))
  (interval a)
  (interval b)
  (interval c)
  (clear-network (timedb-network *timedb*))
  (tassert a b (s))
  (tassert b c (f))
  (tassert a c)
  (what-times))

(defun test6 (&optional (debug? nil))
  (in-timedb (create-timedb "Test Six" *trels-file* debug?))
  (interval a)
  (interval b)
  (interval c)
  (clear-network (timedb-network *timedb*))
  (tassert a b (m))
  (tassert b c (mi))
  (tassert a c)
  (what-times))

(defun test7 (&optional (debug? nil))
  (in-timedb (create-timedb "Test Seven" *trels-file* debug?))
  (interval a)
  (interval b)
  (interval c)
  (clear-network (timedb-network *timedb*))
  (tassert a b (< m))
  (tassert b c (mi >))
  (tassert a c)
  (what-times))
















