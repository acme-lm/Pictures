;;;-*- Mode:Common-Lisp; Package:PICTURES; Base:10 -*-


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


(defvar *view-list* nil)
(defvar *display-list* nil)

(defevent view (:button-press :button-1) (view-select-graphic))
(defevent view (:button-press :button-2) (view-unselect-graphic))
(defevent view (:button-press :button-3) (view-select-region))

;; (defevent view (:key-press #\control-\r)  (refresh-view))
;; (defevent view (:key-press #\meta-\r)     (repair-view))

;; (defevent view (:key-press #\control-\z)  (view-zoom-in))
;; (defevent view (:key-press #\meta-\z)     (view-zoom-out))
;; (defevent view (:key-press #\control-\w)  (view-show-world))

;; (defevent view (:key-press #\control-\b)  (view-pan-right))
;; (defevent view (:key-press #\control-\f)  (view-pan-left))
;; (defevent view (:key-press #\control-\n)  (view-pan-down))
;; (defevent view (:key-press #\control-\p)  (view-pan-up))

;; (defevent view (:key-press #\control-\q)  (handle-keys)) ; to quit

(defevent grabber (:button-press :button-1) (view-transform-graphic))
(defevent grabber (:button-press :button-3) (view-rotate-graphic))


(defparameter *auto-view* nil)


;; color keywords are mapped to black and white for monochrome monitors - override for color monitors
(defun road-demo (&key (a-jump 20)
		    ;; (host *default-host*)
		    (host "localhost")
		    (wall-place 500)
		    (fg "white")
		    (red "white")
		    (blue "black"))
  ;; (declare (special  auto-view test-shell jump block *l* *display-list* *view-list* wall label line graphic-image
  ;; 		     road-view auto wheel1 wheel2 spokes1 spokes2 body road-pict filled-block))
  (let (g-temp)
    (setf jump a-jump)
    (unwind-protect
	(progn
	  (setf body      (make-scene :sensitivity :subselectable))
	  (setf road-pict (make-scene :sensitivity :subselectable))
	  (setf block (make-rectangle 0 0 100 100 ))
	  (setf filled-block (make-filled-rectangle 40 40 90 90 ))
	  (rotate-transform filled-block (radians 30))
	  (setf label (make-label "hot rod crash" :base-x 130 :base-y  40
				  :gstate (make-gstate :foreground "white" :background "black")))
	  ;; label is not inserted into the road-pict scene, it is inserted into the auto scene later
	  (setf line (make-line 50 50 200 200))
	  (scene-insert road-pict line)
	  (scene-insert road-pict block)
	  (move-transform block 30 50)
	  (scene-insert road-pict filled-block)
          (setf  road-view road-pict)
          (scene-insert road-pict (make-line 0 20 950 20))	;; make the roadway
          (scene-insert road-pict (setf auto (make-scene)))	;; make the automobile
	  (scene-insert auto (setf wheel1 (make-scene :sensitivity :subselectable)))	;; make the back wheel
          (scene-insert wheel1 (make-filled-circle 100 30 20 :gstate (make-gstate :foreground fg)))
          (scene-insert wheel1 (setf g-temp (make-filled-circle 100 30 10
								:gstate (make-gstate :foreground blue))))
          (setf spokes1 (make-scene))
          (scene-insert wheel1 spokes1)
          (scene-insert spokes1 (setf g-temp (make-line 100 32 100 36 :gstate (make-gstate :foreground red))))
          (rotate-transform g-temp (radians 45) 100 30)
          (scene-insert spokes1 (setf g-temp (make-line 100 32 100 36 :gstate (make-gstate :foreground red))))
          (rotate-transform g-temp (radians 180) 100 30)
          (scene-insert spokes1 (setf g-temp (make-line 100 32 100 36 :gstate (make-gstate :foreground red))))
          (rotate-transform g-temp (radians 225) 100 30)
          (scene-insert spokes1 (setf g-temp (make-line 100 32 100 36 :gstate (make-gstate :foreground red))))
          (rotate-transform spokes1 (radians 30) 100 30)
          (scene-insert auto (setf wheel2 (make-scene :sensitivity :subselectable)))	;; make the front wheel
          (scene-insert wheel2 (make-filled-circle 100 30 20 :gstate (make-gstate :foreground fg)))
          (scene-insert wheel2 (setf g-temp (make-filled-circle 100 30 10 :gstate (make-gstate :foreground blue))))
	  (setf spokes2 (make-scene))
          (scene-insert wheel2 spokes2)
          (scene-insert spokes2 (setf g-temp (make-line 100 32 100 36 :gstate (make-gstate :foreground red))))
          (rotate-transform g-temp (radians 45) 100 30)
          (scene-insert spokes2 (setf g-temp (make-line 100 32 100 36 :gstate (make-gstate :foreground red))))
          (rotate-transform g-temp (radians 180) 100 30)
          (scene-insert spokes2 (setf g-temp (make-line 100 32 100 36 :gstate (make-gstate :foreground red))))
          (rotate-transform g-temp (radians 225) 100 30)
          (scene-insert spokes2 (setf g-temp (make-line 100 32 100 36 :gstate (make-gstate :foreground red))))
          (move-transform wheel2 150 0)
          (scene-insert auto body)
	  ;; make the body
          (scene-insert body (make-line 112 28 238 28 :gstate (make-gstate :foreground fg)))
          (scene-insert body (make-line 112 30 238 30 :gstate (make-gstate :foreground fg)))
          (scene-insert body (setf g-temp (make-polyline nil :gstate (make-gstate :foreground fg))))
          (setf (vertices g-temp) (vector 270  28 299 28 314.6 34))
          (scene-insert body (make-rectangle 304 33 16 9 :gstate (make-gstate :foreground fg)))
          (scene-insert body (setf g-temp (make-polyline nil :gstate (make-gstate :foreground fg))))
          (setf (vertices g-temp)  (merge-vectors (vector 314.6 314.6 233 185 126 100 60 60)
						  (vector 43    55    68  84  84  68  55 43)))
          (scene-insert body (make-rectangle 54 33 16 9 :gstate (make-gstate :foreground fg)))
          (scene-insert body (setf g-temp (make-polyline nil :gstate (make-gstate :foreground fg))))
          (setf (vertices g-temp) (merge-vectors (vector 62 68 80) (vector 33 28 28)))
          (scene-insert body (setf *l* (make-line 72.5 55 292 55 :gstate (make-gstate :foreground red :line-width 2))))
          (scene-insert body (make-line 74 57 290 57 :gstate (make-gstate :foreground red)))
          (scene-insert body (setf g-temp (make-filled-polygon-edge nil :gstate (make-gstate :foreground red))))
	  (setf (edge-gstate g-temp) (make-gstate :line-width 2 :foreground fg))
          (setf (vertices g-temp) (merge-vectors (vector 227 182 150 150) (vector 68  81  81  68)))
	  (scene-insert body (setf g-temp (make-filled-polygon-edge nil :gstate (make-gstate :foreground red))))
	  (setf (gstate-foreground g-temp) red)
	  (setf (edge-gstate g-temp) (make-gstate :line-width 2 :foreground fg))
          (setf (vertices g-temp) (merge-vectors (vector 145 145 129 106) (vector 68  81  81  68)))
          (scene-insert body (make-rectangle 151 60.5 9 3.5 :gstate (make-gstate :foreground fg)))
          (scene-insert body (make-rectangle 301 46.5 9 3.5 :gstate (make-gstate :foreground fg)))
          (scene-insert body (make-rectangle 66 45.5 9 3.5 :gstate (make-gstate :foreground fg)))
	  (scene-insert auto label) ;; auto scene is :editable
	  (setf *display-list* nil)
	  (setf *view-list* nil)
	  (setf *auto-view* (clue-view-window host))
	  (add-a-scene road-pict)
	  (make-wall road-pict wall-place)
	  (setf (graphic-sensitivity wall) :subselectable)
	  (move-transform auto 100 0)
	  (view-pan *auto-view* 100 0)
	  (add-event label '(:button-press :button-1) '(say-hi ))
	  ;; the event-loop
	  (catch :stop-x-window
	    (loop
	      (process-next-event (contact-display *auto-view*) 0))))
      (close-display (contact-display *auto-view*))
      )))

(defun merge-vectors (vector-x vector-y)
  "merge two vector together by alternating the values"
  (let ((avector (make-array '(10) :adjustable t :fill-pointer 0)))
    (dotimes (i (length vector-x))
      (vector-push-extend (aref vector-x i) avector)
      (vector-push-extend (aref vector-y i) avector))
    avector))

(defun clue-view-window (&optional (default-host *default-host*)
			   (default-display 0)
			   (default-screen 0))
  (declare (special *view-list* *display-list*))
  (let* ((mydisplay (clue:open-contact-display
		     'hello-world
		     :host default-host :display default-display
		     :default-screen default-screen))
	 (screen    (contact-screen (display-root mydisplay)))
	 (mybackground (screen-black-pixel screen))
	 (myforeground (screen-white-pixel  screen))
	 (newwindow (make-contact
		     'top-level-shell
		     :parent mydisplay
		     :state :mapped
		     :x 30
		     :y 30
		     :width 1500
		     :height 300))
	 (mywindow (make-view :parent newwindow :foreground myforeground :background mybackground
			      :title "hello world"  :font "vg-20" :width 700
			      :height 300   :class :input-output
			      :map :on)))
    (break)
    (setf *view-list* (cons mywindow *view-list*))
    (setf *display-list* (cons mydisplay *display-list*))
    ;; (add-event mywindow '(:key-press #\control-\q ) '(handle-keys ))
    ;; (add-event mywindow '(:key-press #\control-\a ) '(key-animate ))
    (add-event mywindow '(:key-press #\a ) '(key-animate ))
    (setf (display-after-function mydisplay) nil)
    mywindow))

(defun clue-exp-view-window (&optional (default-host "uniform")  (default-display 0)  (default-screen 0))

  (let* (( mydisplay (clue:open-contact-display 'hello-world
						:host default-host :display default-display
						:default-screen default-screen))
	 (screen    (contact-screen (display-root mydisplay)))
      	 ( mybackground (screen-black-pixel screen))
	 ( myforeground (screen-white-pixel  screen))

	 ( newwindow (make-contact
		       'top-level-shell
		       :parent mydisplay
		       :state :mapped
		       :x 30
		       :y 30
		       :width 700
		       :height 300))
	 ( mywindow      (make-contact
			   'view
			   :class        :input-output
			   :x            30	;temporary value
			   :y            30	;temporary value
			   :width        400	;temporary value
			   :height       150	;temporary value
			   :parent     newwindow
			   :font       "vg-20"
			   :title      "hello world"
			   :foreground mybackground
			   :background myforeground
			   :map :on)))

    (setf *view-list* (cons mywindow *view-list*))
    (setf *display-list* (cons mydisplay *display-list*))
    ;; (add-event mywindow '(:key-press #\control-q )
    ;; 	       '(handle-keys ))
    ;; (add-event mywindow '(:key-press #\control-a )
    ;; 	       '(key-animate ))
    ))

(defmethod say-hi ((graphic graphic)(view view) &optional (name "graphic"))
  (format t "~a says hi~%" name))

(defmethod handle-keys ((view view))
  (throw :stop-x-window t))

(defmethod key-animate ((view view))
  (declare (special jump))
  (animate jump))

(defun close-views ()
  (dolist (display *display-list* nil)
    (close-display display))
  (setf *view-list* nil)
  (setf *display-list* nil))

(defun add-a-scene (userscene)
  (dolist (view *view-list* nil)
    (setf (view-graphic view) userscene)))

(defun refresh-views ()
  (dolist (view *view-list* nil)
    (refresh-view view) ))

(defun repair-views ()
  (dolist (view *view-list* nil)
    (repair-view view) ))

(defun pan-a-view (view-number x y)
  (view-pan (nth (- view-number 1) *view-list*) x y))

(defun make-wall (road-pict wall-place)
  (declare (special road-view wall brick1 brick2 brick3))

  (setf wall (make-scene)) ; default sensitivity is :editable
  ;; wall sensitivity is later made :subselectable
  (setf brick1 (make-rectangle 250 20 30 30))
  (setf brick2 (make-rectangle 250 20 30 30))
  (move-transform brick2 0 30)
  (setf brick3 (make-rectangle 250 20 30 30))
  (move-transform brick3 0 60)
  (scene-insert wall brick1)
  (scene-insert wall brick2)
  (scene-insert wall brick3)
  (move-transform wall wall-place 0)
  (scene-insert road-pict wall)
  )

(defun move-auto ( jump)
  (declare (special  wheel1 wheel2 spokes1 spokes2 auto wall))
  (let ((dust (make-polypoint nil)))
    (setf (vertices dust)
	  (merge-vectors
	    (vector 58 97 91 3  29 40 94 76 70 36 89 32 87 96 23 88 78 90 6 87)
	    (vector 1  3  13 23 5  5  12 4  1  29 24 23 5  24 13 26 1  16 1 7)))
    (move-transform dust 0 20)
    (scene-insert auto dust)

    (multiple-value-bind (cx1 cy1)
        (graphic-fixed-point spokes1 :center )
      (multiple-value-bind (cx2 cy2)
          (graphic-fixed-point spokes2 :center )
	(print (list cx1 cy1 cx2 cy2))
        (do ()
	    ((> (rect-intersect (graphic-extent auto) (graphic-extent wall)) 0) nil)
          (move-transform auto jump 0)
          (rotate-transform spokes1 (radians -5) cx1 cy1)
          (rotate-transform spokes2 (radians -5) cx2 cy2)
          (repair-views)))
      )
    (scene-delete auto dust)
    (normalize-graphic auto)
    (refresh-views)))

(defun blow-wall ()
  (declare (special auto wall road-pict brick1 brick2 brick3))
  (normalize-graphic road-pict)
  (multiple-value-bind (wx wy)
      (graphic-fixed-point auto :southeast t)
    (dotimes (i 90)
      (move-transform brick1 2.0 -0.2)
      (rotate-transform brick2 (radians -1.38) (+ wx 40) wy)
      (scale-transform brick2 0.992 0.992 wx wy)
      (rotate-transform brick3 (radians +1.38) (- wx 90) wy)
      (scale-transform brick3 1.005 1.005 wx wy)
      (repair-views)))
  (multiple-value-bind (cx2 cy2)
      (graphic-fixed-point brick2 :center)
    (multiple-value-bind (cx3 cy3)
        (graphic-fixed-point brick3 :center)
      (dotimes (i 11)
        (rotate-transform brick2 (radians -5) cx2 cy2)
        (rotate-transform brick3 (radians  5) cx3 cy3)
        (repair-views)))))

(defun animate (jump)
  (move-auto jump)
  (blow-wall))
