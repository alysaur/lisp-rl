;;;; Alysaur trying to make a room.

(defun random-from (low high)
  "Return a random number between the specified low and high limits."
  (+ low (random (- high low -1))))

(defun make-wall (width)
  "Make a horizontal wall of the specified width.
    e.g. '######'"
  (make-list width :initial-element #\#))

(defun make-space-slice (width)
  "Make a slice of empty space of the specified width.
    e.g. '....'"
  (make-list width :initial-element #\.))

(defun make-walled-slice (width)
  "Make a slice of walled space of the specified width.
    e.g. '#....#'"
  (cons #\#
	(nconc (make-space-slice (- width 2))
	       (list #\#))))

(defun make-walled-slices (width height)
  "Make slices of walled space of the specified width and height.
    e.g. '#....#'
         '#....#'
         '#....#'"
  (loop repeat height
     collect (make-walled-slice width)))

(defun make-room (width height)
  "Make a room of the specifed width and height.
    e.g. '######'
         '#....#'
         '#....#'
         '#....#'
         '######'"
  (cons (make-wall width)
	(nconc (make-walled-slices width (- height 2))
	       (list (make-wall width)))))

(defun make-random-room ()
  "Make a room of random width and height."
  (let ((width (random-from 3 50))
	(height (random-from 3 10)))
    (make-room width height)))

(defun draw-room (room)
  "Draw the specified room to standard output."
  (format t "~{~{~a~}~%~}" room))

(defgeneric draw (shape)
  (:documentation "Draw the specified shape to the screen."))

(defmethod draw ((shape room))
  "Draw the specified room to the screen."
  (draw-room (slot-value shape 'data)))

(defclass shape ()
  ((x-position :initarg :x
	       :initform 0
	       :accessor x
	       :documentation "Position of the shape's x-coordinate.")
   (y-position :initarg :y
	       :initform 0
	       :accessor y
	       :documentation "Position of the shape's y-coordinate.")))

(defclass room (shape)
  ((width :initarg :width
	  :initform (random-from 4 71)
	  :accessor width
	  :documentation "Width of the room.")
   (height :initarg :height
	   :initform (random-from 4 13)
	   :accessor height
	   :documentation "Height of the room.")
   data))

(defmethod initialize-instance :after ((room room) &key)
  "Initialize data with a room of the specified width and height."
  (with-slots (width height data) room
    (setf data (make-room width height))))

;;; Test generation of a random room.
(setf *random-state* (make-random-state t))
(draw (make-instance 'room))
