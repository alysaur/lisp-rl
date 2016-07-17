;;;; Alysaur trying to make a level.

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

(defun make-area (width height)
  "Make an area of the specifed width and height.
    e.g. '######'
         '#....#'
         '#....#'
         '#....#'
         '######'"
  (cons (make-wall width)
	(nconc (make-walled-slices width (- height 2))
	       (list (make-wall width)))))

(defun make-random-area ()
  "Make an area of random width and height."
  (let ((width (random-from 3 50))
	(height (random-from 3 10)))
    (make-area width height)))

(defun draw-area (area)
  "Draw the specified area to standard output."
  (format t "~{~{~a~}~%~}" area))

(defgeneric draw (shape)
  (:documentation "Draw the specified shape to the screen."))

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

(defclass area (shape)
  ((width :initarg :width
	  :initform (random-from 4 71)
	  :accessor width
	  :documentation "Width of the area.")
   (height :initarg :height
	   :initform (random-from 4 13)
	   :accessor height
	   :documentation "Height of the area.")
   (data :accessor data
	 :documentation "A list of lists for the tiles in an area.")))

(defmethod initialize-instance :after ((area area) &key)
  "Initialize data with an area of the specified width and height."
  (with-slots (width height data) area
    (setf data (make-area width height))))

(defmethod draw ((shape area))
  "Draw the specified area to the screen."
  (draw-area (slot-value shape 'data)))

(defun has-overlap (area area-list)
  "Check if an area overlaps with a list of areas."
  (with-accessors ((l1 x) (t1 y) (w1 width) (h1 height)) area
    (let ((r1 (+ l1 w1 -1)) (b1 (+ t1 h1 -1)))
      (dolist (n area-list)
	(with-accessors ((l2 x) (t2 y) (w2 width) (h2 height)) n
	  (let ((r2 (+ l2 w2 -1)) (b2 (+ t2 h2 -1)))
	    (if (and (>= r1 l2) (<= l1 r2) (>= b1 t2) (<= t1 b2))
		(return-from has-overlap T)))))))
  nil)

(defclass level (shape)
  ((top :initform 0
	:accessor top
	:documentation "First y-coordinate of the level.")
   (left :initform 0
	 :accessor left
	 :documentation "First x-coordinate of the level.")
   (bottom :initform 0
	   :accessor bottom
	   :documentation "Last y-coordinate of the level.")
   (right :initform 0
	  :accessor right
	  :documentation "Last x-coordinate of the level.")
   (rooms :initform '()
	  :accessor data
	  :documentation "List of areas on the level.")))

(defmethod add ((new-area area) (level level))
  "Add a non-overlapping area to a level."
  (with-accessors ((top top)
		   (left left)
		   (bottom bottom)
		   (right right)
		   (current-areas data)) level
    (with-accessors ((x x)
		     (y y)
		     (width width)
		     (height height)) new-area
      (unless (has-overlap new-area current-areas)
	(setf top (min y top))
	(setf left (min x left))
	(setf bottom (max (+ y height) bottom))
	(setf right (max (+ x width) right))
	(push new-area current-areas)))))

;;; Test generation of a random area.
(setf *random-state* (make-random-state t))
(draw (make-instance 'area))
