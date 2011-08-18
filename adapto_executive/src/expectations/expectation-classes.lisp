(in-package :ad-exe)

;; This file defines the classes of expectations that can be used

;; General expectations: Superclass of all expectations. We might add some common slots there if needed
(defclass expectation () ())

;; Expectations about the position of things defined by an area and and a poseStamped
(defclass position-expectation (expectation)
  ((area :initarg :area :accessor area)
  (pose :initarg :pose :accessor pose)))

(defmethod validate-expectation (x) (error "No validation-function defined for this type"))

;;for the moment return 1 if pose is inside area, 0 if not
(defmethod validate-expectation ((exp position-expectation))
  (if (inside-area (area exp) (pose exp))
    1
    0))