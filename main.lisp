;;;; Alysaur trying to make a room.

;;; Return random number between the specified low and high value.
(defun random-from (low high)
  (+ low (random (- high low -1))))

;;; Make horizontal wall.
;;;  e.g. "######"
(defun make-wall (width)
  (if (= width 1)
    (list "#")
    (append (make-wall (- width 1)) (list "#"))))

;;; Make room space.
;;;  e.g. "...."
(defun make-space-slice (width)
  (if (= width 1)
    (list ".")
    (append (make-space-slice (- width 1)) (list "."))))

;;; Make room space with walls.
;;;  e.g. "#....#"
(defun make-walled-slice (width)
  (append (list "#") (make-space-slice (- width 2)) (list "#")))

;;; Make all room spaces with walls.
;;;  e.g. "#....#"
;;;       "#....#"
;;;       "#....#"
(defun make-walled-slices (width height)
  (if (= height 1)
    (list (make-walled-slice width))
    (append (make-walled-slices width (- height 1)) (list (make-walled-slice width)))))

;;; Make complete room.
;;;  e.g. "######"
;;;       "#....#"
;;;       "#....#"
;;;       "#....#"
;;;       "######"
(defun make-room (width height)
  (append (list (make-wall width)) (make-walled-slices width (- height 2)) (list (make-wall width))))

(defun make-random-room ()
    (let ((width (random-from 4 50))
	   (height (random-from 4 10)))
      (make-room width height)))

;;; Pretty print a room.
(defun print-room (room)
  (format t "~%~{~{~a~}~%~}" room))

;;; Test generation of a random room.
(make-random-state t)
(print-room (make-random-room))
