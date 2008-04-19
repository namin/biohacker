;; -*- Mode: Lisp; -*-

;;;; Description of the Boston Subway system
;;; Last Edited: 1/11/91, KDF

;;; Copyright (c) 1983-1991, Kenneth D. Forbus, Northwestern University,
;;; and Johan de Kleer, the Xerox Corporation.
;;; All rights reserved.

;;; See the file legal.txt for a paragraph stating scope of permission
;;; and disclaimer of warranty.  The above copyright notice and that
;;; paragraph must be included in any separate copy of this file.

(in-package :COMMON-LISP-USER)

;;;   (Note: this map is quite sketchy)
(defline Red-Line)
(defline Green-Line)
(defline Orange-Line)
(defline Blue-Line)

(defstation Airport (Blue-Line) 4.0 1.0)
(defstation Aquarium (Blue-Line) 3.75 0.1)
(defstation Wood-Island (Blue-Line) 5.0 2.0)
(defstation State (Blue-Line Orange-Line) 3.1 -0.75)

(defstation Park-Street (Green-Line Red-Line) 2.5 -0.5)
(defstation Government-Center (green-line blue-line) 2.9 -0.25)
(defstation Copley-Square (Green-Line) 1.0 -1.0)
(defstation Boston-U (Green-Line) -1.0 -1.0)
(defstation North-Station (Green-Line Orange-Line) 2.5 0.75)
(defstation Haymarket (Orange-Line Green-Line) 2.75 0.5)

(defstation South-Station (Red-Line) 3.0 -1.0)
(defstation Washington (Red-Line Orange-Line) 2.75 -0.75)
(defstation Kendall-Square (Red-Line) 1.0 0.0)
(defstation Central-Square (Red-Line) -1.0 0.0)
(defstation Harvard-Square (Red-Line) -2.0 1.0)
