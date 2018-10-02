




(defparameter *display*
  (open-contact-display 'viewing))

(defparameter *top-level-shell*
  (make-contact 'top-level-shell
		:parent *display*
		:state :mapped
		:wm-title "View"
		:x 30
		:y 30
		:width 100
		:height 100))

(defparameter *view*
  (make-view :parent
	     (make-contact
	      'top-level-shell
	      :parent *display*
	      :state :mapped
	      :wm-title "View"
	      :x 30
	      :y 30
	      :width 100
	      :height 100)
	     :width 100
	     :height 100))



(setf (display-after-function (contact-display *view*)) #'display-force-output)
(defparameter *rectangle* (make-filled-rectangle 20 40 35 55))
(draw-graphic *rectangle* *view*)
