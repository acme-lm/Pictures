;;;-*- Mode:Common-Lisp; Package:PICTURES; Base:10 -*-
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

(in-package "PICTURES")


(DEFMACRO  length-point-seq (sequence)
   `(VALUES (FLOOR (length ,sequence) 2)))

(DEFUN print-seq (seq &optional (STREAM t)  return)
  "print the value in the sequence to the stream"

     (when (> (LENGTH seq) 0)
       (FORMAT stream "~%")
	 (DO ((place 0 (1+ place)))
	     ((= place (LENGTH seq)) (RETURN (VALUES) ))
	   (FORMAT stream " ~a" (elt seq place )) (WHEN (AND return (> (MOD place 2) 0))(FORMAT stream "~%"))))
     )


(DEFMETHOD vertex-x ((sequence list) position)
  (declare (type (integer 0 *) position))

    (IF (< position  (length-point-seq sequence))
	(ELT sequence (* position 2))
	(ELT sequence  (* 2 (1- (length-point-seq sequence))))))

(DEFMETHOD vertex-x ((sequence array) position)
  (declare (type (integer 0 *) position))

    (IF (< position  (length-point-seq sequence))
	(ELT sequence (* position 2))
	(ELT sequence  (* 2 (1- (length-point-seq sequence))))))

(DEFMETHOD (SETF vertex-x) (x (sequence array) i)
  (declare (type (integer 0 *) i))
      (IF  (< i  (LENGTH-point-seq sequence))
	   (SETF (elt sequence (* i 2)) x )
	   (PROGN
	     (VECTOR-PUSH-EXTEND x  sequence 5 )
	     (VECTOR-PUSH-EXTEND 0 sequence 5 )
	     x)))

(DEFMETHOD (SETF vertex-x) (x (sequence list) i)
  (declare (type (integer 0 *) i))
      (IF (< i  (LENGTH-point-seq sequence))
	    (SETF (elt sequence (* i 2)) x )
	    (progn
	      (nconc sequence (LIST x  0))
	      x)))


(DEFMETHOD vertex-y ((sequence list) position)

  (IF (< position  (length-point-seq sequence))
	(ELT sequence (1+ (* position 2)))
	(ELT sequence  (1+ (* 2 (1- (length-point-seq sequence)))))))

(DEFMETHOD vertex-y ((sequence array) position)

  (IF (< position  (length-point-seq sequence))
	(ELT sequence (1+ (* position 2)))
	(ELT sequence  (1+ (* 2 (1- (length-point-seq sequence)))))))

(Defmethod (SETF vertex-y) (y (sequence array) i)
  (declare (type (integer 0 *) i))
      (IF (< i  (LENGTH-point-seq sequence))
	    (SETF (elt sequence (1+ (* i 2))) y )
	    (PROGN
	      (VECTOR-PUSH-EXTEND 0 sequence 5 )
	      (VECTOR-PUSH-EXTEND y  sequence 5 )
	      y)))

(Defmethod (SETF vertex-y) (y (sequence list) i)
  (declare (type (integer 0 *) i))
      (IF (< i  (LENGTH-point-seq sequence))
	    (SETF (elt sequence (1+ (* i 2))) y )
	    (progn
	      (nconc sequence (LIST 0  y))
		y)))

(defmethod vertex-i ((sequence list) i)
  (declare (type (integer 0 *) i))
  (VALUES (vertex-x sequence i)(vertex-y sequence i)))


(Defmethod (SETF vertex-i) (point-seq (sequence list) i)
  (declare (type (integer 0 *) i))
  (WHEN (>= (LENGTH point-seq) 2)
    (DO*
      ((pos i (1+ pos))
       (x 0 (+ x 2) )
       (y  (+ x 1) (+ x 1)))
      ((>= y (LENGTH point-seq)))
      (IF (< i (length-point-seq sequence))
	  (progn
	    (SETF (vertex-x sequence pos) (ELT point-seq x))
	    (SETF (vertex-y sequence pos) (ELT point-seq y)))
	  (progn
	    (SETF (vertex-x sequence pos) (ELT point-seq x))
	    (SETF (vertex-y sequence (1- (length-point-seq sequence))) (ELT point-seq y)))))

    point-seq))

(defmethod vertex-i ((sequence array) i)
  (declare (type (integer 0 *) i))
  (VALUES (vertex-x sequence i)(vertex-y sequence i)))


(Defmethod (SETF vertex-i) (point-seq (sequence array) i)
  (declare (type (integer 0 *) i))
  (WHEN (>= (LENGTH point-seq) 2)
    (DO*
      ((pos i (1+ pos))
       (x 0 (+ x 2) )
       (y  (+ x 1) (+ x 1)))
      ((>= y (LENGTH point-seq)))
      (IF (< i (length-point-seq sequence))
	  (progn
	    (SETF (vertex-x sequence pos) (ELT point-seq x))
	    (SETF (vertex-y sequence pos) (ELT point-seq y)))
	  (progn
	    (SETF (vertex-x sequence pos) (ELT point-seq x))
	    (SETF (vertex-y sequence (1- (length-point-seq sequence))) (ELT point-seq y)))))

    point-seq))




(DEFMETHOD vertex-x ((sequence list) position)
  (declare (type (integer 0 *) position))

    (IF (< position  (length-point-seq sequence))
	(ELT sequence (* position 2))
	(ELT sequence  (* 2 (1- (length-point-seq sequence))))))

(DEFMETHOD vertex-x ((sequence array) position)
  (declare (type (integer 0 *) position))

    (IF (< position  (length-point-seq sequence))
	(ELT sequence (* position 2))
	(ELT sequence  (* 2 (1- (length-point-seq sequence))))))

(DEFMETHOD (SETF vertex-x) (x (sequence array) i)
  (declare (type (integer 0 *) i))
      (IF  (< i  (LENGTH-point-seq sequence))
	   (SETF (elt sequence (* i 2)) x )
	   (PROGN
	     (VECTOR-PUSH-EXTEND x  sequence 5 )
	     (VECTOR-PUSH-EXTEND 0 sequence 5 )
	     x)))

(DEFMETHOD (SETF vertex-x) (x (sequence list) i)
  (declare (type (integer 0 *) i))
      (IF (< i  (LENGTH-point-seq sequence))
	    (SETF (elt sequence (* i 2)) x )
	    (progn
	      (nconc sequence (LIST x  0))
	      x)))

(DEFMETHOD vertex-y ((sequence list) position)

  (IF (< position  (length-point-seq sequence))
	(ELT sequence (1+ (* position 2)))
	(ELT sequence  (1+ (* 2 (1- (length-point-seq sequence)))))))

(DEFMETHOD vertex-y ((sequence array) position)

  (IF (< position  (length-point-seq sequence))
	(ELT sequence (1+ (* position 2)))
	(ELT sequence  (1+ (* 2 (1- (length-point-seq sequence)))))))

(Defmethod (SETF vertex-y) (y (sequence array) i)
  (declare (type (integer 0 *) i))
      (IF (< i  (LENGTH-point-seq sequence))
	    (SETF (elt sequence (1+ (* i 2))) y )
	    (PROGN
	      (VECTOR-PUSH-EXTEND 0 sequence 5 )
	      (VECTOR-PUSH-EXTEND y  sequence 5 )
	      y)))

(Defmethod (SETF vertex-y) (y (sequence list) i)
  (declare (type (integer 0 *) i))
      (IF (< i  (LENGTH-point-seq sequence))
	    (SETF (elt sequence (1+ (* i 2))) y )
	    (progn
	      (nconc sequence (LIST 0  y))
		y)))



(DEFMETHOD length-vertices ((sequence list))
  (length-point-seq sequence))

(DEFMETHOD length-vertices ((sequence array))
  (length-point-seq sequence))

(DEFUN point-seq-x-min (point-seq )
  "find the minimum value in a point sequence"
  (LET ((LENGTH (LENGTH point-seq)))
    (DO* ((index 0 (+ 2 index))
	  (value (ELT point-seq 0) (MIN (ELT point-seq index) value)))
 	 ((>= index (- length 2)) (RETURN value))
      )
    ))

(DEFUN point-seq-y-min (point-seq )
  "find the minimum value in a point sequence"
  (LET ((LENGTH (LENGTH point-seq)))
    (DO* ((index 1 (+ 2 index))
	  (value (ELT point-seq 1) (MIN (ELT point-seq index) value)))
	 ((>= index (- length 1)) value))))

(DEFUN point-seq-x-max (point-seq )
  "find the maximum value in a point sequence"
  (LET ((LENGTH (LENGTH point-seq)))
    (DO* ((index 0 (+ 2 index))
	  (value (ELT point-seq 0) (MAX (ELT point-seq index) value)))
	 ((>= index (- length  2)) value))))

(DEFUN point-seq-y-max (point-seq )
  "find the maximum value in a point sequence"
  (LET ((LENGTH (LENGTH point-seq)))
    (DO* ((index 1 (+ 2 index))
	  (value (ELT point-seq 1) (MAX (ELT point-seq index) value)))
	 ((>= index (- length 1)) value))))


(DEFUN find-point-seq-x (point-seq x &key (start 0) (end (1- (length-point-seq point-seq)) ))
  "Find the first occurance of X in the POINT-SEQ and return the POSITION
   START is the point-seq where the search starts, END is where point-seq where the search ends."

  (LET ((start-seq (IF (>= start (length-point-seq point-seq))
		    (- (* (length-point-seq point-seq) 2) 2)
		    (* start 2)))
	(end-seq (IF (>= end (length-point-seq point-seq))
		    (- (* (length-point-seq point-seq) 2) 2)
		    (* end 2))))
    (DO* ((index start-seq  (+ 2 index) )
	  (POSITION  (IF (= x (ELT point-seq index)) (FLOOR (/ index 2)) nil)
		     (IF (= x (ELT point-seq index)) (FLOOR (/ index 2)) position)))
	 ((OR position (>= index end-seq )) position))))



(DEFUN find-point-seq-y (point-seq y &key (start 0) (end (1- (/ (length point-seq) 2))))
  "Find the first occurance of Y in the POINT-SEQ and return the POSITION"

  (LET ((start-seq (IF (>= start (length-point-seq point-seq))
		    (- (* (length-point-seq point-seq) 2) 2)
		    (* start 2)))
	(end-seq (IF (>= end (length-point-seq point-seq))
		    (- (* (length-point-seq point-seq) 2) 1)
		    (+ (* end 2) 1))))
    (DO* ((index  (1+ start-seq) (+ 2 index) )
	  (POSITION  (IF (= y (ELT point-seq index)) (FLOOR (/ index 2)) nil)
		     (IF (= y (ELT point-seq index)) (FLOOR (/ index 2)) position)))
	 ((OR position (>= index end-seq )) position))))


(defmethod insert-vertex ((array array) new-x new-y  i)
  ;; subtract one because vectors have already been extended
  (if (< i (length-point-seq array))
      (progn
	;; push a point on the vector to make sure it is long enough for x
	(vector-push-extend 0 array 5)
	;; push a point on the vector to make sure it is long enough for y
	(vector-push-extend 0 array 5)
	;; count down to the position of insertion
	(do ((index (1- (length array)) (1- index)))
	    ((= (round  index 2) i)
	     (setf (aref array (* i 2)) new-x)
	     (setf (aref array (+ (* i 2) 1)) new-y))
	  ;; store value in vector(x) into vector(x+1)
	  (setf (aref array index) (aref array (- index 2)))))
      (progn
	(vector-push-extend new-x array 5)
	(vector-push-extend new-y array 5) ))
  (values new-x new-y))

(defmethod insert-vertex ((list list ) new-x new-y  i)
  (declare (type wcoord new-x new-y))
  (declare (type integer i))
    (if (< i (length-point-seq list))  ;;subtract one because vectors have already been extended
	(progn
	  (nconc list (list 0 0))
	  (do ((index (1- (length list)) (1- index))) ;count down to the position of insertion
	      ((= (round  index 2) i)
	       (setf (elt list (* i 2)) new-x)
	       (setf (elt  list (+ (* i 2) 1)) new-y))
	    (setf (elt list index) (elt list (- index 2)))))	;store value in vector(x) into vector(x+1)
	(nconc list (list new-x new-y)))
    (values new-x new-y))

(defmethod delete-vertex ( (array array) i)
  (declare (type integer i))
  (let ((point-seq-length (length-point-seq array)))
    (when (and (< i  point-seq-length ) (not (= point-seq-length  0)))
      (multiple-value-bind (x y)  (vertex-i array i)
	(do ((index  (* 2 i)  (1+ index)))
	    ((= index (- (length array) 2))
	     (setf (fill-pointer array) index))      ;move the fill pointer back one to shorten array
	  (setf (elt  array index) (elt array (+ 2 index))))
	(values x y)))))

(defmethod delete-vertex ( (list list) i)
  (declare (type integer i))
  (let ((point-seq-length (length-point-seq list)))
    (when (and (< i  point-seq-length ) (not (= point-seq-length  0)))
      (multiple-value-bind (x y)  (vertex-i list i)
	    (do ((index  (* 2 i)  (1+ index)))
		((= index (- (length list) 2))
		 (nbutlast list) (nbutlast list))
	      (setf (elt  list index) (elt list (+ 2 index))))
      (values x y)))))

(defun min-value-vector (cvector &optional (initial-index 0))
  "find the minimum value in a vector"
  (do ((index (+ 2 initial-index) (+ 2 index))
       (value (elt  cvector initial-index) (min (elt cvector index) value)))
      ((>= index (length cvector)) value)
    )
  )

(defun max-value-vector (cvector &optional (initial-index 0))
  "find the minimum value in a vector"
  (do ((index (+ 2 initial-index) (+ 2 index))
       (value (elt  cvector initial-index) (max (elt cvector index) value)))
      ((>= index (length cvector)) value)
    )
  )
