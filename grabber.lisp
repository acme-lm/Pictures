;;;-*- mode:common-lisp; package:pictures; base:10 -*-


;;; Texas Instruments Incorporated
;;; PO Box 149149
;;; Austin, Texas 78714-9149
;;;
;;; copyright (c)1987,1988,1989,1990 texas instruments incorporated.
;;;
;;; permission is granted to any individual or institution to use,
;;; copy, modify, and distribute this software, provided that this
;;; complete copyright and permission notice is maintained, intact, in
;;; all copies and supporting documentation.
;;;
;;; Texas instruments incorporated provides this software "as is" without
;;; express or implied warranty.
;;;
;;; Authors: Delmar Hager, James Dutton, Teri Crowe
;;; Contributors: Kerry Kimbrough, Patrick hogan, Eric Mielke


(in-package :pictures)


(defparameter  grabber-side-size 6
  "this is the size of the width and the height of the grabber")

(defun round-vector (vector)
  "round all of the values of a vector"
  (dotimes (pointer (fill-pointer vector) nil)
    (setf (elt vector pointer) (floor  (elt vector pointer) ))))

(defclass selection-scene (scene)
  ((grabber-size    :type   integer
		    :initform grabber-side-size
		    :accessor grabber-size
		    :documentation "the size of the grabber for the grabber-rect"))
  (:documentation "the graphic class for representing the selection scene"))

(defmacro scene-visible-p (graphic)
  `(and (not (and (and min-x min-y width height)
		  ;; was optional rect given
		  (not (graphic-within-p ,graphic min-x min-y width height))
		  (not (graphic-intersects-p ,graphic min-x min-y width height))))
	(viewable-p ,graphic)))

(defmethod draw-graphic ((scene selection-scene) (view view)
                           &optional min-x min-y width height)
  (declare (ignore  min-x min-y width height))
  (with-slots (elements extent) scene
    (when (viewable-p scene)
	(graphic-world-transform scene)	; cache our transform
	(dotimes (position (length elements))
	  (draw-graphic (elt elements position) view )))))

(defun   make-selection-scene ( &rest options )
   (apply #'make-instance 'selection-scene
			     :sensitivity :subselectable
			     options))

(defclass grabber (filled-rectangle)
  ((name           :type (or null symbol)
		   :initarg :grabber-name
		   :accessor grabber-name
		   :documentation "the position name of the grabber in the the grabber-rect")

   (opposing
    :initform nil
    :accessor opposing-grabber
    :documentation "the grabber that is opposite of this instance of grabber"))
  (:documentation "this is special class of filled rectangle to represent the grabbers of the extent rectangle"))

(defmethod scene-insert ((scene selection-scene) graphic &optional pos )
  (declare (ignore pos))
  ;; To a scene
  (with-slots (elements parent) scene
    ;; Insert after last graphic
    (vector-push-extend graphic elements 1))
  graphic)

(defun  normalize-transform (transform)
  (when transform
     (with-slots (t11 t12 t21 t22 t31 t32) transform
       (setf
	 t11 1
	 t12 0
	 t21 0
	 t22 1
	 t31 0
	 t32 0))))

(defmethod draw-graphic ((rectangle grabber) (view view)
			 &optional min-x min-y width height)
  (declare (ignore  min-x min-y width height))
  (when (viewable-p rectangle)
    (let ((grabber-rect-transform (grabber-rect-transform view)))
      (with-slots (vertices transform) rectangle
	(with-vector temp-vertices
	  (copy-to-vector vertices temp-vertices)
	  (normalize-transform transform)
	  (scale-transform transform (t11 grabber-rect-transform) (t22 grabber-rect-transform )
			   (rectangle-origin-x rectangle)(rectangle-origin-y rectangle) )
	  (transform-point-seq transform temp-vertices)
	  (view-draw-filled-polygon view	; no, use draw-polygon to draw it
				    temp-vertices
				    (graphic-gstate rectangle)))))))

(defclass grabber-rect (scene)
  (

  (graphic          :type   (or null graphic)
		    :initform nil
		    :accessor grabber-graphic
		    :documentation "the graphic the grabber-rect is attached to")
  (north-grabber          :type   (or null grabber)
		    :initform nil
		    :accessor north-grabber
		    :documentation "the north grabber of the grabber-rect")

  (south-grabber          :type   (or null grabber)
		    :initform nil
		    :accessor south-grabber
		    :documentation "the south grabber of the grabber-rect")

  (east-grabber          :type   (or null grabber)
		    :initform nil
		    :accessor east-grabber
		    :documentation "the east grabber of the grabber-rect")

  (west-grabber          :type   (or null grabber)
		    :initform nil
		    :accessor west-grabber
		    :documentation "the west grabber of the grabber-rect")

  (northwest-grabber          :type   (or null grabber)
		    :initform nil
		    :accessor northwest-grabber
		    :documentation "the northwest grabber of the grabber-rect")

  (northeast-grabber          :type   (or null grabber)
		    :initform nil
		    :accessor northeast-grabber
		    :documentation "the northeast grabber of the grabber-rect")

  (southwest-grabber          :type   (or null grabber)
		    :initform nil
		    :accessor southwest-grabber
		    :documentation "the southwest grabber of the grabber-rect")

  (southeast-grabber          :type   (or null grabber)
		    :initform nil
		    :accessor southeast-grabber
		    :documentation "the southeast grabber of the grabber-rect")

  (background-grabber          :type   (or null grabber)
		    :initform nil
		    :accessor background-grabber
		    :documentation "the background grabber of the grabber-rect")
    )
  (:documentation "the graphic class for representing the grabber rectangle"))

(defmethod draw-graphic ((scene grabber-rect) (view view)
                           &optional min-x min-y width height)
  (declare (type (or null wcoord) min-x min-y width height))
  (with-slots (elements extent) scene
    (when (viewable-p scene)
	(graphic-world-transform scene)	; cache our transform
	(dotimes (position (length elements))
	  (draw-graphic (elt elements position) view min-x min-y width height)
	  ))))


(defclass background-grabber ( grabber)
  ()
  (:documentation "this is special  class of rectangle to represent the background grabber"))

;define a graphic-contains-p method for the background-grabber that will use the graphic-contains-p
;method of the the selected graphic

(defmethod graphic-contains-p ((grabber background-grabber) x y &optional pixel)
  (declare (type wcoord x y ))
  (graphic-contains-p (grabber-graphic (graphic-parent grabber)) x y pixel))

(defmethod graphic-contains-p ((filled-polygon grabber) x y &optional pixel)
  (declare (ignore pixel))
  (declare (type wcoord x y))
					;  (when (point-in-extents-p filled-polygon x y)
  (with-slots (vertices) filled-polygon
    (with-vector temp-vertices
      (copy-to-vector vertices temp-vertices)
      (transform-point-seq (graphic-world-transform filled-polygon) temp-vertices)
      (when
	  (inside-p temp-vertices (make-point :x x :y y)) t))))

(defun make-background-grabber (name x y width height transform)
  (funcall  #'make-instance 'background-grabber
	 :allow-other-keys t
	 :grabber-name name
	 :vertices (complete-rectangle x y (+ x width) (+ y height))
	 :transform transform))

(defmethod initialize-instance :after ((background-grabber background-grabber) &key vertices)
    (setf (vertices background-grabber) vertices))

(defun make-grabber (name x y width height transform
		     &rest options
		     &key &allow-other-keys)
  (apply #'make-instance 'grabber
	 :allow-other-keys t
	 :grabber-name name
	 :vertices (complete-rectangle x y (+ x width) (+ y height))
	 :transform transform
	 :gstate (make-gstate :function boole-xor)
	 options))

(defmethod initialize-instance :after ((grabber grabber) &key vertices)
    (setf (vertices grabber) vertices))

(defmethod (setf grabber-graphic) (agraphic (grabber-rect grabber-rect))
  (with-slots (graphic transform extent sensitivity) grabber-rect
    (setf graphic agraphic)
     (if agraphic
	(progn
	  (extent-compute grabber-rect)
	  (setf sensitivity :subselectable))
	(setf sensitivity :hidden)
	  )))

(defmacro scale-value (fixed-p p1 p2)
  `(if (eq ,fixed-p ,p1)
       0.000001
       (/ (- ,fixed-p ,p2)
	  (- ,fixed-p ,p1))))


(defmacro grabber-corner-p (grabber-name)
  `(or (eq ,grabber-name 'northwest-grabber)
	      (eq ,grabber-name 'southwest-grabber)
	      (eq ,grabber-name 'northeast-grabber)
	      (eq ,grabber-name 'southeast-grabber)))

(defmacro grabber-north-south-p (grabber-name)
  `(or (eq ,grabber-name 'north-grabber)
	      (eq ,grabber-name 'south-grabber)))

(defmacro grabber-east-west-p (grabber-name)
  `(or (eq ,grabber-name 'east-grabber)
	      (eq ,grabber-name 'west-grabber)))


(defun make-grabber-rect (view &key (highlight 1) parent  )
  (let* ((size (grabber-size (view-selection-scene view))))
	 (funcall  #'make-instance 'grabber-rect
			      :sensitivity :subselectable
			      :gstate (make-gstate :foreground highlight :function boole-xor )	;the function is set for xor
			      :size size
			      :parent parent
			      :view view
			      )))

(defmacro grabber-transform ()
  `(copy-transform transform (make-transform)))

(defmethod initialize-instance :after ((grabber-rect grabber-rect) &key size view)
  (let* ((sign-x (signum (view-scale-x view)))
	(sign-y (signum (view-scale-y view)))
	(negsize (- size))
	(width (* sign-x size))
	(negwidth (* sign-x negsize))
	(heigth (* sign-y size))
	(negheigth (* sign-y negsize))
	(transform (grabber-rect-transform view)))
    (with-slots (north-grabber northwest-grabber northeast-grabber
			       south-grabber southwest-grabber southeast-grabber
			       east-grabber west-grabber center-grabber background-grabber) grabber-rect
      (scene-insert grabber-rect
		    (setf background-grabber (make-background-grabber  'background-grabber 0 0 0 0 (grabber-transform) )))
      (scene-insert grabber-rect
		    (setf  northwest-grabber (make-grabber  'northwest-grabber 0 0 width negheigth (grabber-transform) )))
      (scene-insert grabber-rect
		    (setf north-grabber (make-grabber  'north-grabber 0 0 width negheigth (grabber-transform) )))
      (scene-insert grabber-rect
		    (setf west-grabber (make-grabber  'west-grabber 0 0 width heigth (grabber-transform) )))
      (scene-insert grabber-rect
		    (setf east-grabber (make-grabber  'east-grabber 0 0 negwidth heigth (grabber-transform)) ))
      (scene-insert grabber-rect
		    (setf south-grabber (make-grabber  'south-grabber 0 0 width heigth (grabber-transform) )))
      (scene-insert grabber-rect
		    (setf southwest-grabber (make-grabber  'southwest-grabber 0 0 width heigth (grabber-transform) )))
      (scene-insert grabber-rect
		    (setf southeast-grabber (make-grabber  'southeast-grabber 0 0 negwidth heigth (grabber-transform) )))
      (scene-insert grabber-rect
		    (setf northeast-grabber (make-grabber  'northeast-grabber 0 0 negwidth negheigth (grabber-transform))))
      (setf (opposing-grabber northwest-grabber)  southeast-grabber
	    (opposing-grabber northeast-grabber) southwest-grabber
	    (opposing-grabber north-grabber) south-grabber
	    (opposing-grabber south-grabber) north-grabber
	    (opposing-grabber east-grabber) west-grabber
	    (opposing-grabber west-grabber) east-grabber
	    (opposing-grabber southeast-grabber) northwest-grabber
	    (opposing-grabber southwest-grabber) northeast-grabber
	    (opposing-grabber background-grabber) northeast-grabber))
    grabber-rect))

(defmacro get-motion-notify-event (grabber-name fixed-x fixed-y px py)
  `(cond
    ((grabber-corner-p ,grabber-name)
      (list 'scale-rubberband ,fixed-x  ,fixed-y))
    ((grabber-north-south-p ,grabber-name)
      (list 'scale-rubberband-fixed-width  ,fixed-x  ,fixed-y  ,px))
    ((grabber-east-west-p ,grabber-name)
     (list 'scale-rubberband-fixed-height  ,fixed-x ,fixed-y ,py))
    ((eq ,grabber-name 'background-grabber)
      '(move-box))))

(defmacro grabber-x (grabber-name grabber view)
  `(cond
    ((grabber-north-south-p ,grabber-name)
     (transform-x ,view (rectangle-origin-x (east-grabber (graphic-parent ,grabber)))) )
    ((eq 'background-grabber grabber-name) (transform-x ,view (rectangle-origin-x ,grabber)))
    (t (transform-x ,view (rectangle-origin-x ,grabber)))))

(defmacro grabber-y (grabber-name grabber view)
  `(cond
    ((grabber-east-west-p ,grabber-name)
     (transform-y ,view (rectangle-origin-y (north-grabber (graphic-parent ,grabber)))) )
    ((eq 'background-grabber grabber-name) (transform-y ,view (rectangle-origin-y ,grabber)))
    (t (transform-y ,view (rectangle-origin-y   ,grabber)))))




(defmethod view-transform-graphic ((grabber grabber) (view view) &key (event :button-1))
  (let* ((pixel-size (view-pixel-size view))
	 (grabber-name (grabber-name grabber))
	 (opposing (opposing-grabber grabber))
	 (fixed-x  (fixed-x opposing view ))
	 (fixed-y  (fixed-y opposing view))
	 (px1 (grabber-x grabber-name grabber view))
	 (py1 (grabber-y grabber-name grabber view))
	 (highlight-color (view-highlight-color view))
	 (selection-elements (scene-elements (view-selection-scene view)))
	 )
    (when (editable-p (grabber-graphic (graphic-parent grabber)))
      (with-event (x y display)
	(let* ((*px* px1  )                   ( *py*  py1)
	       (*fixed-x* fixed-x)            (*fixed-y* fixed-y)
	       (*delta-fx* (- fixed-x x  ))   ( *delta-fy* (-  fixed-y y))
	       (*delta-px* (- x *px* ))       (*delta-py* (- y *py*)) )
	  (declare (special *px* *py* *fixed-x* *fixed-y* *delta-fx* *delta-fy* *delta-px* *delta-py* *transform*))
	  (with-event-mode
	    (view (list `(:motion-notify ,event)(get-motion-notify-event grabber-name fixed-x fixed-y px1 py1))
				 `((:button-release ,event) (view-button-release t)))
	    (when (eql (first (get-motion-notify-event grabber-name fixed-x fixed-y px1 py1)) 'move-box)
	      (when (= *px* *fixed-x*) (setf *px* (floor (+ *px* pixel-size))
					     *delta-px* (floor (+ *delta-px* pixel-size))))
	      (when (= *py* *fixed-y*) (setf *py* (floor (+ *py* pixel-size))
					     *delta-py* (floor (+ *delta-py* pixel-size))))
	      )

	    (process-motion-notify-events view display *fixed-x* *fixed-y* *px* *py* highlight-color)
	    (erase-grabber-rects view  selection-elements (fill-pointer  selection-elements))
	    (repair-view view)

	    (if (eq grabber-name 'background-grabber)
		(move-selected-graphics view selection-elements (fill-pointer  selection-elements)
					(- (view-untransform-x view *fixed-x*) (view-untransform-x view fixed-x ))
					(-  (view-untransform-y view *fixed-y*) (view-untransform-y view fixed-y)))

		(scale-selected-graphics
		  view selection-elements (fill-pointer  selection-elements)
		  (scale-value *fixed-x* px1 *px*)
		  (scale-value *fixed-y* py1 *py*)
		  (grabber-name opposing))
		)
	    )))
      (repair-view view))))


(defmethod view-move-graphic ((grabber grabber) (view view) &key (event :button-1))
  (let* ((pixel-size (view-pixel-size view))
	 (grabber-name (grabber-name grabber))
	 (opposing (opposing-grabber grabber))
	 (fixed-x  (fixed-x opposing view ))
	 (fixed-y  (fixed-y opposing view))
	 (px1 (grabber-x grabber-name grabber view))
	 (py1 (grabber-y grabber-name grabber view))
	 (highlight-color (view-highlight-color view))
	 (selection-elements (scene-elements (view-selection-scene view)))
	 )

  (with-event (x y display)
    (let* ((*px* px1  )                   ( *py*  py1)
	   (*fixed-x* fixed-x)            (*fixed-y* fixed-y)
	   (*delta-fx* (- fixed-x x  ))   ( *delta-fy* (-  fixed-y y))
	   (*delta-px* (- x *px* ))       (*delta-py* (- y *py*)) )

      (declare (special *px* *py* *fixed-x* *fixed-y* *delta-fx* *delta-fy* *delta-px* *delta-py* *transform*))
      (when (= *px* *fixed-x*) (setf *px* (floor (+ *px* pixel-size))
				     *delta-px* (floor (+ *delta-px* pixel-size))))
      (when (= *py* *fixed-y*) (setf *py* (floor (+ *py* pixel-size))
				     *delta-py* (floor (+ *delta-py* pixel-size))))
      (with-event-mode (view `((:motion-notify ,event) (move-box))
			     `((:button-release ,event) (view-button-release t)))
	(process-motion-notify-events view display *fixed-x* *fixed-y* *px* *py* highlight-color)
	(erase-grabber-rects view  selection-elements (fill-pointer  selection-elements))
	(repair-view view)

	(move-selected-graphics view selection-elements (fill-pointer  selection-elements)
				(- (view-untransform-x view *fixed-x*) (view-untransform-x view fixed-x ))
				(-  (view-untransform-y view *fixed-y*) (view-untransform-y view fixed-y))))))
   (repair-view view)))



(defmethod view-scale-graphic ((grabber grabber) (view view) &key uniform (event :button-1))
  (let* (
	 (grabber-name (grabber-name grabber))
	 (opposing (opposing-grabber grabber))
	 (fixed-x  (fixed-x opposing view ))
	 (fixed-y  (fixed-y opposing view))
	 (px1 (grabber-x grabber-name grabber view))
	 (py1 (grabber-y grabber-name grabber view))
	 (highlight-color (view-highlight-color view))
	 (selection-elements (scene-elements (view-selection-scene view)))
	 )
    (when (editable-p (grabber-graphic (graphic-parent grabber)))
      (with-event (x y display)
	(let* ((*px* px1  )                   ( *py*  py1)
	       (*fixed-x* fixed-x)            (*fixed-y* fixed-y)
	       (*delta-fx* (- fixed-x x  ))   ( *delta-fy* (-  fixed-y y))
	       (*delta-px* (- x *px* ))       (*delta-py* (- y *py*)) )

	  (declare (special *px* *py* *fixed-x* *fixed-y* *delta-fx* *delta-fy* *delta-px* *delta-py* *transform*))
	  (with-event-mode (view
			     (list `(:motion-notify ,event)(get-motion-notify-event grabber-name fixed-x fixed-y px1 py1))
			     `((:button-release ,event) (view-button-release t)))
	    (unless (eq grabber-name 'background-grabber)
	      (process-motion-notify-events view display *fixed-x* *fixed-y* *px* *py* highlight-color))
	    (erase-grabber-rects view  selection-elements (fill-pointer  selection-elements))
	    (repair-view view)
	    (scale-selected-graphics
	      view selection-elements (fill-pointer  selection-elements)
	      (scale-value *fixed-x* px1 *px*)
	      (if uniform
		  (scale-value *fixed-x* px1 *px*)
		  (scale-value *fixed-y* py1 *py*))
	      (grabber-name opposing))
	    )))))
  (repair-view view))



(defun  fixed-x (opposing view)
  (let ((grabber-class-name (grabber-name opposing))
	(parent (graphic-parent opposing)))
    (cond
      ((grabber-corner-p grabber-class-name)
       (transform-x view (rectangle-origin-x opposing)))
      ((eq 'west-grabber grabber-class-name)
       (transform-x view (rectangle-origin-x (west-grabber parent))))
      ((eq 'east-grabber grabber-class-name)
       (transform-x view (rectangle-origin-x (east-grabber  parent))))
      ((eq 'north-grabber grabber-class-name)
       (transform-x view (rectangle-origin-x (west-grabber  parent))))
      ((eq 'south-grabber grabber-class-name)
       (transform-x view (rectangle-origin-x (west-grabber  parent)))))))

(defun fixed-y (opposing view)
  (let ((grabber-class-name (grabber-name opposing))
	(parent (graphic-parent opposing)))
    (cond
      ((grabber-corner-p grabber-class-name)
       (transform-y view (rectangle-origin-y opposing)))
      ((eq 'west-grabber grabber-class-name)
       (transform-y view (rectangle-origin-y (southwest-grabber  parent))))
      ((eq 'east-grabber grabber-class-name)
       (transform-y view (rectangle-origin-y (southeast-grabber  parent))))
      ((eq 'north-grabber grabber-class-name)
       (transform-y view (rectangle-origin-y (north-grabber  parent))))
      ((eq 'south-grabber grabber-class-name)
       (transform-y view (rectangle-origin-y (south-grabber  parent)))))))



(defun erase-grabber-rects ( view selection-elements selection-length )
  (dotimes (pos  selection-length)
    (let* ((highlight (elt selection-elements pos))
	   (graphic (grabber-graphic highlight)))
      (unless (eq (graphic-sensitivity highlight) :hidden)	;is the grabber-rect being used?
	(graphic-damage graphic)
	(draw-graphic highlight view))))
  (repair-view view)
  )

(defun scale-selected-graphics (view selection-elements selection-length x-scale y-scale fixed-point)
  (declare (ignore view))
  (dotimes (pos  selection-length)
	  (let* ((highlight (elt selection-elements pos))
		 (graphic (grabber-graphic highlight)) )
	    (unless (eq (graphic-sensitivity highlight) :hidden)	 ;is the grabber-rect being used?
	      (multiple-value-bind (fx fy)
		  (graphic-fixed-point graphic fixed-point)
		(scale-transform graphic x-scale y-scale fx fy)
		(graphic-extent graphic)
		(extent-compute highlight ))))))

(defun move-selected-graphics (view selection-elements selection-length delta-x delta-y)
  (declare (ignore view))
  (dotimes (pos  selection-length)				;move the graphics in the scene
	    (let* ((highlight (elt selection-elements pos))
		   (graphic (grabber-graphic highlight)) )
	      (when (eq (graphic-sensitivity highlight) :hidden) (return))
	      (when graphic						;is the graphic attached to a grabber rectangle.
		(move-transform graphic					;  yes, move the graphic.
				delta-x delta-y)
		(graphic-extent graphic)
		(extent-compute highlight )))))



(defmethod view-rotate-graphic ((grabber grabber) (view view) &key (event :button-3))
  (unless (or (grabber-north-south-p (grabber-name grabber))(grabber-east-west-p (grabber-name grabber)))
    (let* (
	   (selection-elements (scene-elements (view-selection-scene view)))
	   (highlight-color (view-highlight-color view))
	   (opposing (opposing-grabber grabber))
	   (fixed-x  (fixed-x opposing view ))
	   (fixed-y  (fixed-y opposing view))
	   (px1  (transform-x view (rectangle-origin-x grabber)))
	   (py1  (transform-y view (rectangle-origin-y grabber))))

      (when (editable-p (grabber-graphic (graphic-parent grabber)))
	(unwind-protect
	    (progn
	      (grab-pointer view #.(make-event-mask :button-release)
			    :owner-p t)
	      (with-event (x y display)
		(let ((*px* px1 ) ( *py*  py1 ) (*fixed-x* fixed-x)(*fixed-y* fixed-y)
		      (*transform* (make-transform)))

		  (declare (special *px* *py* *fixed-x* *fixed-y* *delta-fx* *delta-fy* *delta-px* *delta-py* *transform*))

		  (with-special-vector *rotate-vector*
		    (setf (display-after-function display) #'display-force-output)

		    (setf (fill-pointer *rotate-vector*) 10)
		    (setf (elt *rotate-vector* 0) *fixed-x*    (elt *rotate-vector* 1) *fixed-y*
			  (elt *rotate-vector* 2) *px*         (elt *rotate-vector* 3) *fixed-y*
			  (elt *rotate-vector* 4) *px*         (elt *rotate-vector* 5) *py*
			  (elt *rotate-vector* 6) *fixed-x*    (elt *rotate-vector* 7) *py*
			  (elt *rotate-vector* 8) *fixed-x*    (elt *rotate-vector* 9) *fixed-y*)
		    (with-event-mode (view `((:motion-notify ,event) (rotate-box))
					   `((:button-release ,event) (view-button-release t)))
		      (drawlines-with-gc view highlight-color *rotate-vector* )
		      (catch :release
			(loop
			  (process-next-event display )))
		      (transform-point-seq *transform* *rotate-vector*)
		      (round-vector *rotate-vector*)
		      (drawlines-with-gc view highlight-color *rotate-vector*)
		      (setf (display-after-function display) nil)
		      (erase-grabber-rects view  selection-elements (fill-pointer  selection-elements))
		      (repair-view view)
		      ))
		  (rotate-selected-graphics
		    view selection-elements (fill-pointer  selection-elements)
		    (rotation	(view-untransform-x view *fixed-x*)(view-untransform-y view *fixed-y*)
				(rectangle-origin-x grabber)(rectangle-origin-y grabber)
				(view-untransform-x view *px*)(view-untransform-y view *py*))
		    (grabber-name opposing)))))
	  (ungrab-pointer (contact-display view))))

      (repair-view view))))

(defun rotation (fixed-x fixed-y x y x2 y2)
  "determine the difference in the angle between fixed-x:fixed-y x1:y1 and fixed-x:fixed-y x2:y2."

  (- (if (= x2 fixed-x)
	    (radians 0)
	    (+ (atan (/ (-   y2 fixed-y) (-  x2 fixed-x )))
	       (if (< (* (signum (-  x fixed-x ))(-  x2 fixed-x )) 0) (radians 180) 0)))
	(if (= fixed-x x)
	    0
	    (atan (/ (-  y fixed-y ) (-  x fixed-x ))))))




(defun rotate-selected-graphics (view selection-elements selection-length rotation fixed-point)
  (declare (ignore view))
  (dotimes (pos  selection-length)				;rotate the graphics in the scene
	    (let* ((highlight (elt selection-elements pos))
		   (graphic (grabber-graphic highlight)) )
	      (when (eq (graphic-sensitivity highlight) :hidden) (return))
	      (when graphic						;is the graphic attached to a grabber rectangle?
		(multiple-value-bind (fx fy)
		  (graphic-fixed-point graphic fixed-point)
		  (rotate-transform graphic				;  yes, rotate the graphic.
				   rotation fx fy)

		(extent-compute highlight )
		)))))








;method: extent-compute
;  compute the extent rectangle for the graphic.  the method should be defined
;  for each derived graphic class.  the primary method returns nil, meaning
;  "undefined extent."

;  note: a graphic's extent rectangle is defined in the local coordinate system.  this
;  means that each graphic should apply its own transform to its computed extent
;  before returning it.  to obtain the extent of a graphic in the world coordinate system,
;  call the world-extent method (defined below).

(defmethod extent-compute ((grabber-rect grabber-rect))


  (with-slots (graphic  extent) grabber-rect
    (when  graphic
      (let* ((graphic-extent (world-extent graphic))
	    (view (first (graphic-views graphic)))
	    (size (* -1 (/ (grabber-size (view-selection-scene view)) (view-scale view) 2.0))))
	(extent-copy graphic-extent extent)
	(let* ((xmax (extent-rect-xmax extent))
	       (xmin (extent-rect-xmin extent))
	       (ymin (extent-rect-ymin extent))
	       (ymax (extent-rect-ymax extent)))
	  (dolist (grab '(northwest-grabber north-grabber east-grabber northeast-grabber
			  southwest-grabber south-grabber west-grabber southeast-grabber))
		  (setf (graphic-sensitivity (funcall grab grabber-rect)) :editable))
	  (when (= xmax xmin) (setf xmax (+ xmin (view-pixel-size view)))
		(dolist (grab '(north-grabber east-grabber northeast-grabber
			  southwest-grabber west-grabber south-grabber))
		  (setf (graphic-sensitivity (funcall grab grabber-rect)) :hidden)))
	  (when (= ymax ymin) (setf ymax (+ ymin (view-pixel-size view)))
		(dolist (grab '(north-grabber east-grabber northeast-grabber
			  southwest-grabber west-grabber south-grabber))
		  (setf (graphic-sensitivity (funcall grab grabber-rect)) :hidden)))
	  (let* ((width (/ (- xmax xmin) 2.0))
		 (height (/ (- ymax ymin) 2.0)))
	    (setf (rectangle-origin-x (northwest-grabber grabber-rect)) xmin)
	    (setf (rectangle-origin-y (northwest-grabber grabber-rect)) ymax)
	    (setf (rectangle-origin-x (southwest-grabber grabber-rect)) xmin)
	    (setf (rectangle-origin-y (southwest-grabber grabber-rect)) ymin)
	    (setf (rectangle-origin-x (northeast-grabber grabber-rect)) xmax)
	    (setf (rectangle-origin-y (northeast-grabber grabber-rect)) ymax)
	    (setf (rectangle-origin-x (southeast-grabber grabber-rect)) xmax)
	    (setf (rectangle-origin-y (southeast-grabber grabber-rect)) ymin)

	    (setf (rectangle-origin-x (north-grabber grabber-rect)) (+ xmin width size))
	    (setf (rectangle-origin-y (north-grabber grabber-rect)) ymax)
	    (setf (rectangle-origin-x (south-grabber grabber-rect)) (+ xmin width size))
	    (setf (rectangle-origin-y (south-grabber grabber-rect)) ymin)
	    (setf (rectangle-origin-x (east-grabber grabber-rect)) xmax)
	    (setf (rectangle-origin-y (east-grabber grabber-rect)) (+ ymin height size))
	    (setf (rectangle-origin-x (west-grabber grabber-rect)) xmin)
	    (setf (rectangle-origin-y (west-grabber grabber-rect)) (+ ymin height size))

	    (setf (rectangle-origin-x (background-grabber grabber-rect)) xmin)
	    (setf (rectangle-origin-y (background-grabber grabber-rect)) ymin)

	    ))))
    extent))



(defmethod extent-compute ((grabber grabber))

  (with-slots (vertices) grabber
    (let  ((x-min (rectangle-origin-x grabber))
	   (y-min (rectangle-origin-y grabber)))
      (multiple-value-setq (x-min y-min )
	(transform-point (graphic-transform grabber) x-min y-min ))
      (setf (rectangle-origin-x grabber) x-min)
      (setf (rectangle-origin-y grabber) y-min)
      (make-extent-rect
	:xmin (point-seq-x-min vertices)
	:ymin (point-seq-y-min vertices)
	:xmax (point-seq-x-max vertices)
	:ymax (point-seq-y-max vertices))
      )))
