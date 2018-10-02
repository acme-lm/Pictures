;;;-*- mode:common-lisp; package:pictures; base:10 -*-
;;;
;;;
;;;
;;;			 texas instruments incorporated
;;;				  p.o. box 149149
;;;			       austin, texas 78714-9149
;;;
;;; copyright (c)1987,1988,1989,1990 texas instruments incorporated.
;;;
;;; permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; texas instruments incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
;;; authors: delmar hager, james dutton, teri crowe
;;; contributors: kerry kimbrough, patrick hogan, eric mielke

(in-package :pictures)


(defparameter  *an-extent-rectangle* (make-extent-rect))

;private macro: valid-xcoord
;  determine whether the given var is a valid coordinate for the x window system



;function: make-view
;  return a new view object.

(defun make-view (&rest options &key &allow-other-keys)
  "make a new view.
the following keyword options are allowed: gravity resize-extent-p scale origin-x origin-y graphic"


  (apply #'make-contact 'view options))

;basic contact methods:

(defun get-contact-background (view)
  (if view
       (let ((background (contact-background view)))
	 (cond
	   ((numberp background) background)
	   ((pixmap-p background) 0)
	   ((eql background :none) 0)
	   ((eql background :parent-relative) (get-contact-background (contact-parent view)))))
       0))

(defmethod initialize-instance :after ((view view) &key)
  (with-slots (grabber-rect-transform) view
    (setf grabber-rect-transform (make-transform))))

(defmethod realize :after ((view view))

  (with-slots (default-gcontext selection highlight-color) view
    (setf default-gcontext (create-gcontext :drawable view ))
    (let ((black (screen-black-pixel (contact-screen view)))
	    (white (screen-white-pixel (contact-screen view)))
	    (background (get-contact-background view)))

	(if
	  (eql  background black)
	    (progn
	      (setf (gcontext-foreground default-gcontext) white )
	      (setf (gcontext-background default-gcontext) black ))
	    (progn
	      (setf (gcontext-foreground default-gcontext) black )
	      (setf (gcontext-background default-gcontext) white )))
	(setf highlight-color (logxor (gcontext-foreground default-gcontext)(gcontext-background default-gcontext)))


      )

    (let ((selection-scene (make-selection-scene)))
      (setf (graphic-view selection-scene)  view)	;attach the view-selection to a view
      (setf (view-selection-scene view) selection-scene)
      (dotimes (place 10)
	(scene-insert selection-scene (make-grabber-rect view :parent selection-scene
							 :highlight (view-highlight-color view)))
	)
      (with-slots (elements parent) selection-scene
	(setf (fill-pointer elements) 0)
	(setf parent nil))

      (with-slots (view-graphic) view
	(unless view-graphic
	  (setf view-graphic (make-scene :sensitivity :subselectable))
	  (setf (graphic-view view-graphic) view))))))



(defmethod resize :around ((view view) width height border-width)
  (with-slots (origin-x origin-y gravity ) view
     (multiple-value-bind (gravity-x gravity-y) (gravity-point view gravity)
	(call-next-method)
	(multiple-value-bind (new-gravity-x new-gravity-y)(gravity-point view gravity)
	  (setf origin-x (- origin-x (- new-gravity-x gravity-x)))
	  (setf origin-y (- origin-y (- new-gravity-y gravity-y)))))))

;method: display
;  display the view contact after an exposure, etc.

(defmethod display ((view view) &optional (x 0) (y 0) (width (contact-width view))
		     (height (contact-height view)) &key)

  (with-slots (view-graphic damage-count origin-x origin-y  (contact-height height) default-gcontext gcontext) view
    (let ((scale-x (view-scale-x view))
	  (scale-y (view-scale-y view))
	  (scale (view-scale view))
	  (pixel (view-pixel-size view)))

    (when view-graphic
      (if (and (= x 0) (= y 0) (= width (contact-width view)) (= height (contact-height view)))
	  (progn
	    (setf damage-count 0)
	    (refresh-view view))
	  (progn
	    (view-damage view	; notify damage control
			 (+ origin-x (- pixel) (/ x scale-x))
			 (+ origin-y (- pixel) (/ (- contact-height y height) scale-y))
			 (float (+ (/ width scale)  pixel))
			 (float (+ (/ height scale) pixel)))
	    (repair-view view)))))))	; go repair the damage

;view attribute methods:
(defmethod gravity-point ((view view) gravity)
  (declare (type (member :northwest :north  :northeast
                         :west      :center :east
                         :southwest :south  :southeast)
                 gravity))
    (let* ((extent (world-extent view))			; get the extent
           (xmin (extent-rect-xmin extent))
           (xmax (extent-rect-xmax extent))
           (ymin (extent-rect-ymin extent))
           (ymax (extent-rect-ymax extent))
           (xmid (/ (+ xmin xmax) 2.0))			; compute mid points
           (ymid (/ (+ ymin ymax) 2.0)))
      (case gravity					; return the appropriate coord
        (:northwest	(values xmin ymax))
        (:north		(values xmid ymax))
        (:northeast	(values xmax ymax))
        (:west		(values xmin ymid))
        (:center	(values xmid ymid))
        (:east		(values xmax ymid))
        (:southwest	(values xmin ymin))
        (:south		(values xmid ymin))
        (:southeast	(values xmax ymin)))))


(defmethod view-orientation ((view view) &key (x 1) (y 1))
  (setf (view-scale-x  view) (* (signum x) (view-scale-x view))
	(view-scale-y  view) (* (signum y) (view-scale-y view))
	(slot-value (grabber-rect-transform view) 't11) (* (signum x) (slot-value (grabber-rect-transform view) 't11) )
	(slot-value (grabber-rect-transform view) 't22) (* (signum x) (slot-value (grabber-rect-transform view) 't22) )))

;method: view-gravity
;  returns or changes the current view-gravity.  the view-gravity is the
;  alignment point of the graphic's bounding rectangle that remains fixed (in
;  world coordinates) after the view window is resized.

(defmethod view-gravity ((view view))


  (with-slots (gravity) view
    gravity))

(defmethod (setf view-gravity) (gravity (view view))


  (with-slots ((view-gravity gravity)) view
    (setf view-gravity gravity)))


(defmethod view-scale ((view view))
  (abs (view-scale-x view)))

(defmethod (setf view-scale) (x (view view))
  (multiple-value-bind (pan-x pan-y) (gravity-point view (view-gravity view))

    (setf (view-scale-x view) (* (signum (view-scale-x view)) x)
	  (view-scale-y view) (* (signum (view-scale-y view)) x)
	  (slot-value (grabber-rect-transform view) 't11) (* (signum (view-scale-x view)) (/ 1 x) )
	  (slot-value (grabber-rect-transform view) 't22) (* (signum (view-scale-y view)) (/ 1 x) ))
    (view-pan view pan-x pan-y))
  x)

;method: view-pan
;  change the view so that the given point (x, y) (in world coordinates) is located
;  according to the given align point of the view window.

(defmethod view-pan ((view view) x y &optional (gravity (view-gravity view)))
  (declare (type (member :northwest :north  :northeast
                         :west      :center :east
                         :southwest :south  :southeast)
                 gravity))

  (with-slots (width height origin-x origin-y ) view
    (let* (
	   (scale-x (view-scale-x view))
	   (scale-y (view-scale-y view))
	   (wc-width  	   (float (/ width scale-x)))
           (wc-height 	   (float (/ height scale-y)))
           (wc-half-width  (float (/ wc-width 2)))
           (wc-half-height (float (/ wc-height 2))))
      (case gravity
        (:northwest	(setf origin-x x
                              origin-y (- y wc-height)))
        (:north		(setf origin-x (- x wc-half-width)
                              origin-y (- y wc-height)))
        (:northeast	(setf origin-x (- x wc-width)
                              origin-y (- y wc-height)))
        (:west		(setf origin-x x
                              origin-y (- y wc-half-height)))
        (:center	(setf origin-x (- x wc-half-width)
                              origin-y (- y wc-half-height)))
        (:east		(setf origin-x (- x wc-width)
                              origin-y (- y wc-half-height)))
        (:southwest	(setf origin-x x
                              origin-y y))
        (:south		(setf origin-x (- x wc-half-width)
                              origin-y y))
        (:southeast	(setf origin-x (- x wc-width)
                              origin-y y))))))


(defmethod view-show-world ((view view))

  (let ((extent (world-extent (view-graphic view))))
    (unless (= (- (extent-rect-xmax extent)(extent-rect-xmin extent))
	       (- (extent-rect-ymax extent) (extent-rect-ymin extent)) 0)
      (setf (view-scale view) (min (/ (contact-width view)
				      (- (extent-rect-xmax extent)(extent-rect-xmin extent)))
				   (/ (contact-height view)
				      (- (extent-rect-ymax extent) (extent-rect-ymin extent))))))

      (view-pan view (extent-rect-xmin extent)(extent-rect-ymin extent) )
      (refresh-view view)))



(defmethod view-show-region ((view view) extent)
  (unless (= (- (extent-rect-xmax extent)(extent-rect-xmin extent))
	     (- (extent-rect-ymax extent) (extent-rect-ymin extent)) 0)

    (setf (view-scale view) (min (/ (contact-width view)
				  (- (extent-rect-xmax extent)(extent-rect-xmin extent)))
			       (/ (contact-height view)
				  (- (extent-rect-ymax extent) (extent-rect-ymin extent))))))

  (let*
    ((extent-xmin (extent-rect-xmin extent))
     (extent-ymin (extent-rect-ymin extent))
     (extent-width (- (extent-rect-xmax extent)(extent-rect-xmin extent)) )
     (extent-height (-  (extent-rect-ymax extent)(extent-rect-ymin extent))))
    (multiple-value-bind (x y) (case (view-gravity view)

				 (:southwest  (values  extent-xmin
						       extent-ymin  ))
				 (:northwest  (values  extent-xmin
						       (+ extent-ymin  extent-height) ))
				 (:south      (values  (+ extent-xmin  (/ extent-width 2.0))
						       extent-ymin))
				 (:north      (values  (+ extent-xmin  (/ extent-width 2.0))
						       (+ extent-ymin  extent-height)))
				 (:west       (values  extent-xmin
						       (+ extent-ymin  (/ extent-height 2.0))))
				 (:center     (values  (+ extent-xmin  (/ extent-width 2.0))
						       (+ extent-ymin  (/ extent-height 2.0))))
				 (:southeast  (values  (+ extent-xmin extent-width)
						       extent-ymin))
				 (:northeast  (values  (+ extent-xmin extent-width)
						       (+ extent-ymin  extent-height)))
				 (:east       (values  (+ extent-xmin extent-width)
						       (+ extent-ymin  (/ extent-height 2.0))))
				 (t           (values   extent-xmin
							extent-ymin)))
      (view-pan view x y))

    ))

;method: view-damage
;  records a damaged region of the view for later repair. the damaged-region
;  contains either a single graphic object (in which case the damaged region is
;  given by the object's extent) or a world coordinate list of the form
;  (min-x min-y width height).

(defmethod view-damage ((view view) &rest damaged-region)
  (with-slots (damage-count damage) view
    (let ((new-damage (make-extent-rect))	; compute damage extent rectangle
	  (view-extent (world-extent view))
          min-union max-intersect
          min-union-area
          (max-intersect-area 0))
      (if (typep (car damaged-region) 'graphic)
	  (progn
	    (unless (extent-valid-p (car damaged-region))
	      (graphic-extent (car damaged-region)))
	    (world-extent (car damaged-region) new-damage))

          (setf (extent-rect-xmin new-damage) (first  damaged-region)
		(extent-rect-ymin new-damage) (second damaged-region)
		(extent-rect-xmax new-damage) (+ (first damaged-region)
						 (third  damaged-region))
		(extent-rect-ymax new-damage) (+ (second damaged-region)
						 (fourth damaged-region))))
      (when
	(not (or
		   (> (extent-rect-xmin new-damage) (extent-rect-xmax view-extent))
		   (> (extent-rect-ymin new-damage) (extent-rect-ymax view-extent))
		   (< (extent-rect-xmax new-damage) (extent-rect-xmin view-extent))
		   (< (extent-rect-ymax new-damage) (extent-rect-ymin view-extent))))

	(dotimes (i damage-count)		; calculate min-union and max-intersect
	  (let ((union-area     (rect-union new-damage (aref damage i)))
		(intersect-area (rect-intersect new-damage (aref damage i))))

	    (when (or (null min-union)
		      (< union-area min-union-area))
	      (setf min-union-area union-area)
	      (setf min-union i))

	    (when (> intersect-area max-intersect-area)
	      (setf max-intersect-area intersect-area)
	      (setf max-intersect i))))

	(cond
	  ((> max-intersect-area 0)		; it intersected with something
	   (rect-merge new-damage		; use the largest such intersection
		       (aref damage max-intersect)))

	  ((< damage-count max-damage)		; no intersection, room for more?
	   (incf damage-count)			; yes, add it to the list
	   (setf (aref damage (- damage-count 1)) new-damage))

	  (t					; otherwise, just use the smallest union
	   (rect-merge new-damage
		       (aref damage min-union))))))))


;method: view-damaged-p
;  returns true if there are damaged regions to repair. may be used with setf to
;  reset the view's damage. if the new value is false, then any previous damage
;  is ignored; otherwise, the new value is ignored.

(defmethod view-damaged-p ((view view))


  (with-slots (damage-count) view
    (eql damage-count 0)))


(defmethod (setf view-damaged-p) (damaged (view view))
  (declare (type boolean damaged))


  (with-slots (damage-count) view
    (unless damaged (setf damage-count 0))))


;method: view-graphic
;  returns or changes the scene associated with the given view.



(defmethod  (setf view-graphic)  :after (view-graphic (view view)  )
  (declare (type (or null graphic) view-graphic))

  (with-slots ((graphic view-graphic) )  view
    (setf graphic view-graphic)
    (with-slots (views) view-graphic
      (push view views))))


;method: view-pixel-size
;  return the world-coordinate size of a pixel for the given view.

(defmethod view-pixel-size ((view view))


  (let ( (scale (view-scale view)))
    (/ 1 scale)))


;method: refresh-view
;  redraws the entire view scene and clears any damages.

(defmethod refresh-view ((view view))

  (with-slots (view-graphic origin-x origin-y width height  damage-count) view
    (let ((scale-x (view-scale-x view))
	  (scale-y (view-scale-y view)))
      (clear-area view)
      (display-force-output (contact-display view))
      (setf damage-count 0)
      (when view-graphic
	(draw-graphic view-graphic view origin-x origin-y
		      (float (/ width scale-x))
		      (float (/ height scale-y)))
	(display-force-output (contact-display view))
	))
    ))

(defmethod refresh-view :after ((view view))
      (draw-graphic (view-selection-scene view) view)
 )

;method: repair-view
;  redraws any damaged regions in the view and clears any damages.

(defmethod repair-view ((view view))

  (with-slots (damage-count damage view-graphic default-gcontext selection) view
   (dotimes (i damage-count)				; for each damage rectangle
      (let ((xmin (extent-rect-xmin (elt damage i)))	; store corners locally
            (ymin (extent-rect-ymin (elt damage i)))
            (xmax (extent-rect-xmax (elt damage i)))
            (ymax (extent-rect-ymax (elt damage i)))
            (pixel (view-pixel-size view)))
      (multiple-value-bind (clip-xmin clip-ymin)	; compute clipping rectangle
          (transform-point view xmin ymin)
        (multiple-value-bind (clip-xmax clip-ymax)
            (transform-point view xmax ymax)
	  (when (< clip-xmax clip-xmin)
	    (rotatef clip-xmin clip-xmax))
	  (when (< clip-ymax clip-ymin)
	    (rotatef clip-ymin clip-ymax))			; view coordinates are third quadrant

          (clear-area view
                      :x clip-xmin
                      :y clip-ymin
                      :width  (- clip-xmax clip-xmin -1)
                      :height (- clip-ymax clip-ymin -1))
          (draw-graphic-clipped view-graphic view		; draw view-graphic within damaged area
                        (- xmin (* 2 pixel))
                        (- ymin (* 2 pixel))
                        (- xmax xmin (- (* 4 pixel)))
                        (- ymax ymin (- (* 4 pixel))))
          (display-force-output (contact-display view))
          (setf (gcontext-clip-mask default-gcontext)	; get rid of clip-mask
                :none)))))

    (setf damage-count 0))				; clear the damages
  (draw-graphic (view-selection-scene view) view)		; draw the highlight objects
  )




;method: view-scale-point
;  convert the given x-distance and y-distance to equivalent distances in the
;  view coordinate system.  if graphic-world-transform is given, apply it to the
;  distances before converting to view coordinates.

(defmethod view-scale-point ((view view) x-distance y-distance
                                       &optional graphic-world-transform)
  (declare (type (or null transform) graphic-world-transform))


  (with-slots ( height) view
    (let ((scale-x (view-scale-x view))
	  (scale-y (view-scale-y view)))
    (multiple-value-bind (world-x world-y)
        (scale-point graphic-world-transform x-distance y-distance)
      (values (floor (* world-x scale-x))
              (floor (* world-y scale-y)))))))


;method: transform-point
;  convert the given x and y object coordinates to view coordinates for the
;  given view.  if graphic-world-transform is given, apply it to the point before
;  converting to view coordinates.
(defmethod transform-point ((view view) x y )


  (with-slots (origin-x origin-y scale-x scale-y height) view

      (values (floor (* (- x origin-x) scale-x))
              (floor (- height  (* (- y origin-y) scale-y))))))

(defmethod transform-x ((view view) x  )

  (with-slots (origin-x origin-y scale-x height) view
     (floor (* (- x origin-x) scale-x))))

(defmethod transform-y ((view view)  y )

  (with-slots (origin-x origin-y scale-y height) view
    (floor (- height (* (- y origin-y) scale-y)))))

(defmethod view-transform-vector ((view view) vertices &optional round)
  "this function destructively changes the value of vertices by applying the view transform to them"
  (with-slots (origin-x origin-y scale-x scale-y height) view
    (if round
	(do ((i 0 (+ i 2)))
	    ((>= i (length vertices)))
	  (setf (elt vertices i) (round (* (- (elt vertices i) origin-x) scale-x)))
	  (setf (elt vertices (1+ i))       (- height (round (* (- (elt vertices (1+ i)) origin-y) scale-y)))))

	(do ((i 0 (+ i 2)))
	    ((>= i (length vertices)))
	  (setf (elt vertices i) (floor (* (- (elt vertices i) origin-x) scale-x)))
	  (setf (elt vertices (1+ i)) (floor (- height  (* (- (elt vertices (1+ i)) origin-y) scale-y))))))
    (values vertices)))


(defmethod view-untransform-point ((view view) window-x  window-y)

  (values
    (+ (origin-x view) (/ window-x (view-scale-x view)))		;change to world coordinates
    (- (+ (origin-y view) (/ (contact-height view) (view-scale-y view)))
       (/ window-y (view-scale-y view))))  ;change from 4th quadrant view
  )                                                                    ;coordinates to 1st quadrant world coordinate

(defmethod untransform-point ((view view) window-x  window-y)

  (values
    (+ (origin-x view) (/ window-x (view-scale-x view)))		;change to world coordinates
    (- (+ (origin-y view) (/ (contact-height view) (view-scale-y view)))
       (/ window-y (view-scale-y view))))  ;change from 4th quadrant view
  )                                                                    ;coordinates to 1st quadrant world coordinate


(defmethod view-untransform-x ((view view) window-x )
  						;change to x world coordinates
    (+ (origin-x view) (/ window-x (view-scale-x view)))			;change from 4th quadrant view coordinates
    )									;to 1st quadrant world coordinate


(defmethod view-untransform-y ((view view) window-y )

    (- (+ (origin-y view) (/ (contact-height view) (view-scale-y view)))	;change to y world coordinates
       (/ window-y (view-scale-y view))))					;change from 4th quadrant view coordinates




;method: view-zoom
;  change the scale of the view. the horizontal and vertical scale are changed
;  uniformly. if absolute-p is true, then scale is an absolute scale factor;
;  otherwise, scale is multiplied with the current scale to form the new scale.
;  fixed-point is a point in the view that will remain fixed after the scale is
;  performed.  any of the nine possible alignment points may be specified and
;  the default is :southwest.

(defmethod view-zoom ((view view) scale &key  (absolute-p nil) (fixed-point :southwest))
  (declare (type (and number (satisfies plusp)) scale))
  (declare (type boolean absolute-p))
  (declare (type (or list (member :northwest :north  :northeast
                         :west      :center :east
                         :southwest :south  :southeast))
                 fixed-point))
  (let (fixed-point-x fixed-point-y)
    (if (listp fixed-point)
	(progn
	  (setf fixed-point-x (first fixed-point))
	  (setf fixed-point-y (second fixed-point)))

	(multiple-value-setq (fixed-point-x fixed-point-y)	; remember the fixed point
	  (gravity-point view fixed-point)))
      (if absolute-p
          (setf (view-scale view)  scale)		;   absolutely
          (setf (view-scale view ) (* (view-scale view) scale)))	;   relatively
;    (unless (eq fixed-point :southwest)			; pan back to the fixed point
      (view-pan view fixed-point-x fixed-point-y	;   if we need to
                fixed-point)))


;method: world-extent
;  return the extent of the given view in world coordinates.  a nil value means that
;  the extent is undefined.  if result-extent is provided, it is used to store the result
;  extent.  otherwise, a new extent-rect is created and returned.

(defmethod world-extent ((view view) &optional result-extent)
  (declare (type (or null extent-rect) result-extent))


  (with-slots (origin-x origin-y  width height) view
    (unless (or (zerop width) (zerop height))
      (let ((new-extent (or result-extent (make-extent-rect)))
	    (scale (view-scale view)))
        (setf (extent-rect-xmin new-extent) origin-x
              (extent-rect-ymin new-extent) origin-y
              (extent-rect-xmax new-extent) (+ origin-x (float (/ width scale)))
              (extent-rect-ymax new-extent) (+ origin-y (float (/ height scale))))
        new-extent))))





;private function: rect-union
;  return the area of the union of rect1 and rect2.

(defun rect-union (rect1 rect2)
  (declare (type extent-rect rect1 rect2))

  (* (- (max (extent-rect-xmax rect1)
             (extent-rect-xmax rect2))
        (min (extent-rect-xmin rect1)
             (extent-rect-xmin rect2)))
     (- (max (extent-rect-ymax rect1)
             (extent-rect-ymax rect2))
        (min (extent-rect-ymin rect1)
             (extent-rect-ymin rect2)))))

;private function: rect-intersect
;  return the area of the intersection of rect1 and rect2.

(defun rect-intersect (rect1 rect2)
  (declare (type extent-rect rect1 rect2))

  (let ((x-length (- (min (extent-rect-xmax rect1)
                          (extent-rect-xmax rect2))
                     (max (extent-rect-xmin rect1)
                          (extent-rect-xmin rect2))))
        (y-length (- (min (extent-rect-ymax rect1)
                          (extent-rect-ymax rect2))
                     (max (extent-rect-ymin rect1)
                          (extent-rect-ymin rect2)))))
    (if (and (plusp x-length)
             (plusp y-length))
        (* x-length y-length)
        0)))


;private function: rect-merge
;  merge (union) rect1 and rect2 and modify rect2 to contain the result.

(defun rect-merge (rect1 rect2)
  (declare (type extent-rect rect1 rect2))

  (setf (extent-rect-xmax rect2)
        (max (extent-rect-xmax rect1)
             (extent-rect-xmax rect2))

        (extent-rect-xmin rect2)
        (min (extent-rect-xmin rect1)
             (extent-rect-xmin rect2))

        (extent-rect-ymax rect2)
        (max (extent-rect-ymax rect1)
             (extent-rect-ymax rect2))

        (extent-rect-ymin rect2)
        (min (extent-rect-ymin rect1)
             (extent-rect-ymin rect2)))
  rect2)
