;;;-*- Mode:Common-Lisp; Package:PICTURES; Base:10 -*-



;;; Texas Instruments Incorporated
;;; PO BOX 149149
;;; Austin, Texas 78714-9149
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is"
;;; without express or implied warranty.
;;;
;;; Authors: Delmar Hager, James Dutton, Teri Crowe
;;; Contributors: Kerry Kimbrough, Patrick Hogan, Eric Mielke


(in-package "PICTURES")


(defmethod  view-zoom-value ((view view))
  "the amount the view by"
  (getf (slot-value view 'plist) :zoom))


(defmethod  (setf view-zoom-value) ( zoom (view view) )
  "set the amount to zoom the view by"
  (if zoom
      (setf (getf (slot-value view 'plist) :zoom) zoom)
      (remf (slot-value view 'plist) :zoom)))

(defmethod  view-zoom-x ((view view))
  "the fixed x value to zoom at"
  (getf (slot-value view 'plist) :zoom-x))


(defmethod  (setf view-zoom-x) ( zoom-x (view view) )
  "the fixed x value to zoom at"
  (if zoom-x
      (setf (getf (slot-value view 'plist) :zoom-x) zoom-x)
      (remf (slot-value view 'plist) :zoom-y)))

(defmethod  view-zoom-y ((view view))
  "the fixed x value to zoom at"
  (getf (slot-value view 'plist) :zoom-y))


(defmethod  (setf view-zoom-y) (zoom-y (view view) )
  "the fixed x value to zoom at"
  (if zoom-y
      (setf (getf (slot-value view 'plist) :zoom-y) zoom-y)
      (remf (slot-value view 'plist) :zoom-y)))

(defmethod  view-zoom-gravity ((view view))
  "the gravity point at which to zoom "
  (getf (slot-value view 'plist) :zoom-gravity))


(defmethod  (setf view-zoom-gravity) ( gravity (view view) )
  "set the gravity point at which to zoom "
  (assert  (member gravity '( :northwest :north  :northeast
                         :west      :center :east
                         :southwest :south  :southeast))
	   (gravity)
	    "enter a new value of  :northwest :north :northeast :west  :center :east :southwest :south :southeast"
	     )
  (setf (getf (slot-value view 'plist) :zoom-gravity) gravity))

(defmethod view-zoom-in ((view view))
  (view-zoom view (or (view-zoom-value view) 2)
	     :fixed-point (or (and (view-zoom-x view)(view-zoom-y view)(list (view-zoom-x view)(view-zoom-y view)))
				   (view-zoom-gravity view)
				   :center))
  (refresh-view view))

(defmethod view-zoom-out ((view view))
  (view-zoom view (or (and (view-zoom-value view) (/ 1 (view-zoom-value view) )) (/ 1 2))
	     :fixed-point (or (and (view-zoom-x view)(view-zoom-y view)(list (view-zoom-x view)(view-zoom-y view)))
				   (view-zoom-gravity view)
				   :center))
  (refresh-view view))
