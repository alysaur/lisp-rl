;;;; Alysaur trying to make a floor.

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

(defmethod translate ((shape shape) dx dy)
  "Translate a shape by the specified offsets."
  (with-accessors ((x x) (y y)) shape
    (setq x (+ x dx))
    (setq y (+ y dy))))

(defclass room (shape)
  ((width :initarg :width
	  :initform (random-from 4 71)
	  :accessor width
	  :documentation "Width of the room.")
   (height :initarg :height
	   :initform (random-from 4 13)
	   :accessor height
	   :documentation "Height of the room.")
   (data :accessor data
	 :documentation "A list of lists for the tiles in a room.")))

(defmethod initialize-instance :after ((room room) &key)
  "Initialize data with a room of the specified width and height."
  (with-slots (width height data) room
    (setf data (make-room width height))))

(defclass floor (shape)
  ((top :initform 0
	:accessor top
	:documentation "First y-coordinate of the floor.")
   (left :initform 0
	 :accessor left
	 :documentation "First x-coordinate of the floor.")
   (bottom :initform 0
	   :accessor bottom
	   :documentation "Last y-coordinate of the floor.")
   (right :initform 0
	  :accessor right
	  :documentation "Last x-coordinate of the floor.")
   (rooms :initform '()
	  :accessor data
	  :documentation "List of rooms on the floor.")))

(defmethod add ((room room) (floor floor))
  (with-accessors ((top top)
		   (left left)
		   (bottom bottom)
		   (right right)
		   (data data)) floor
    (with-accessors ((x x)
		     (y y)
		     (width width)
		     (height height)) room
      ;; TODO: check for no overlap
      (setf top (min y top))
      (setf left (min x left))
      (setf bottom (max (+ y height) bottom))
      (setf right (max (+ x width) right))
      (push room data))))

;;; Test generation of a random room.
(setf *random-state* (make-random-state t))
(draw (make-instance 'room))
