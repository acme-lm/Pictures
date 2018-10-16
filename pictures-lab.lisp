

;; (ql:quickload :pictures)
;;(in-package :cluei)
(in-package :pictures)



;; class: WM-SHELL
;; class: TOP-LEVEL-SHELL


;; (dolist (class (class-name-precedence-list class-name) 0)
;; 	 (let ((init-mask (getf (rest (assoc :event-mask (clue-resources class))) :initform)))
;; 	   (when init-mask
;; 	     (return init-mask))))
;; (class-name-precedence-list 'TOP-LEVEL-SHELL)







(defparameter *rectangle* (make-filled-rectangle 20 40 35 55))
(defparameter *line* (make-line 30 30 70 30))
(defparameter *polygon* (make-polygon '(10 10 50 10 50 30 25 50 10 30)))
(defparameter *view* nil)

(defun run-describe ()
  (describe *polygon*))

(defun show-graphic ()
  (PROGN
    (SETF clue:*default-host* "freyr")
    (setf *view* (make-view  :parent
			     (make-contact
			      'top-level-shell
			      :parent (clue:open-contact-display 'hello-world)
			      :state :mapped
			      :wm-title "View"
			      :x 300
			      :y 300
			      :width 400
			      :height 400)
			     :width 400
			     :height 400))
    (SETF (contact-background *view*) (screen-white-pixel
				       (contact-screen *view*)))
    (process-next-event (contact-display *view*))
    (SETF (display-after-function (contact-display *view*)) #'display-force-output)
    (draw-graphic *rectangle* *view*)
    (draw-graphic *polygon* *view*)))
