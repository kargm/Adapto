(in-package :ad-exe)

;; define area classes for validation if a pose is inside an area

;; superclass of all areas
(defclass area () ())

;; circle defined by x,y and radius
(defclass circle (area)
  ((x :initarg :x :accessor x)
   (y :initarg :y :accessor y)
   (radius :initarg :radius :accessor radius)))

;; functions to validate if a pose is inside of an area
;; (defun inside-area (area pose)
;;   (cond ((typep area 'circle)
;;           (< (cl-transforms:v-dist (cl-transforms:make-3d-vector (x pose) (y pose) 0) (cl-transforms:make-3d-vector (x area) (y area) 0)) (radius area)))))

(defmethod inside-area (area pose) (error "No inside-area function defined for this kind of area"))

(defmethod inside-area ((area circle) pose)
  (< (cl-transforms:v-dist (cl-transforms:make-3d-vector (x pose) (y pose) 0) (cl-transforms:make-3d-vector (x area) (y area) 0)) (radius area)))