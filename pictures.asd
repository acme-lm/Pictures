;;;-*- Mode:Common-Lisp; Package:user; Base:10 -*-
;;;
;;;
;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
;;; Authors: Delmar Hager, James Dutton, Teri Crowe
;;; Contributors: Kerry Kimbrough, Patrick Hogan, Eric Mielke

(in-package :cl-user)

(defvar *pictures-directory*  "/arc/lw/pictures/")
(defvar *pictures-source*   "/arc/lw/pictures/source/")

(asdf:defsystem pictures
  :depends-on (clio)
  :description "Common Lisp 2-D Graphics Library"
  :version "1.0.1"
  :components
  ((:file "package")
   (:file "types" :depends-on ("package"))
   (:module caches
  	    :depends-on ("class-definitions")
  	    :pathname ""
  	    :components
  	    ((:file "defgeneric")
	     (:file "edge")
	     (:file "macros")))
   (:module class-definitions
  	    :depends-on ("types")
  	    :pathname ""
  	    :components
  	    ((:file "class-def")
	     (:file "font-family")))
   (:module base
  	    :depends-on ("caches")
  	    :pathname ""
  	    :components
  	    ((:file "sequence")
	     (:file "gstate")
	     (:file "transform")))
   (:file "gstack" :depends-on ("base"))
   (:file "graphic" :depends-on ("extent"))
   (:file "extent" :depends-on ("gstack"))
   (:file "scene" :depends-on ("graphic"))
   (:file "view" :depends-on ("extent"))
   (:module view-methods
  	    :depends-on ("view")
  	    :pathname ""
  	    :components
  	    ((:file "view-select")
	     (:file "view-pan")
	     (:file "view-draw")
	     (:file "view-events")))
   (:file "line" :depends-on ("graphic"))
   (:file "circle" :depends-on ("graphic"))
   (:file "polypoint" :depends-on ("graphic"))
   (:file "polygon" :depends-on ("polypoint"))
   (:module rectangle
  	    :depends-on ("polygon")
  	    :pathname ""
  	    :components
  	    ((:file "rectangle")
	     (:file "bspline")
	     (:file "ellipse")))
   (:file "label" :depends-on ("graphic"))
   (:file "gimage" :depends-on ("graphic"))
   (:file "gevents" :depends-on ("graphic"))
   (:file "grabber" :depends-on ("rectangle"))
   (:file "zoom" :depends-on ("view-methods"))
   (:module save
  	    :depends-on ("caches")
  	    :pathname ""
  	    :components
	    ((:file "utilities")
	     (:file "save")
	     (:file "restore")))))
