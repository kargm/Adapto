(in-package :adapto-desig)

;;; This file provides a ROS subscription to the /map topic,
;;; to help finding valid locations for the robot to stand based on the ros occupancy grid.

(defun make-fluent-setter-callback (fluent)
  "util function returning a callback function setting the fluent to a whatever it is called with"
  (lambda (msg)
    (setf (value fluent) msg)))

;; TODO: all global fluents that represent the env should be in one place, like global vars, but with call traceability
(defvar *map-fl* (make-fluent :name '*map-fluent* :value 'waiting-for-data-maybe-you-should-startup-ros)
  "Newest map received on the map topic.")

(defun ros-location-costmaps-init ()
  "Subscribes to the map topic a callback that writes into fluent."
  (ros-info adapto-ros "Subscribing to map topic.")
  (subscribe (roslisp:get-param "~map-topic" "/map")
             "nav_msgs/OccupancyGrid"
             (make-fluent-setter-callback *map-fl*))
  (ros-info adapto-ros "Subscribed to map topic."))

;; makes sure init is called on (startup-ros)
(register-ros-init-function ros-location-costmaps-init)
