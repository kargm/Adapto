(in-package :adapto-desig)

;; This package contains location designator resolution rules, driven
;; by costmaps. They are based on the CRAM-highlevel package
;; location-costmap, then functions and PROLOG predicates defined
;; therein.
;;
;; The key idea is to provide a costmap for the costmap reasoning by
;; creating a predicate:
;; (<- (desig-costmap ?desig ?cm) ...
;; based on designator properties. This needs to bind ?cm to an object
;; of type location-costmap (if the designator fits).
;;
;;
;;
;; Code tested with:
;; (with-designators
;;     ((obj-loc (location `((pose
;;                            ,(tf:make-pose-stamped
;;                              "/map" 0.0
;;                              (tf:make-3d-vector 1.0 1.0 1.3)
;;                              (tf:euler->quaternion :az (/ pi 2.0)))))))
;;      (stand-loc
;;       (location `((close-to ,obj-loc)))))
;;   (reference stand-loc))

;; heigt map for only one height
(defclass fixed-height-map ()
  ((height :reader height
           :initform 0.0
           :initarg :height)))

(defmethod height-map-lookup ((map fixed-height-map) x y)
  (height map))


(defmethod costmap-generator-name->score ((name (eql 'free-space)))
  10)
(defmethod costmap-generator-name->score ((name (eql 'close-space)))
  4)

(defun grid->costmap (cm-1)
  "creates a costmap from a ROS occupancy-grid object"
  (let ((map
         (make-instance 'location-costmap
                        :width (grid-width cm-1)
                        :height (grid-height cm-1)
                        :origin-x (origin-x cm-1)
                        :origin-y (origin-y cm-1)
                        :resolution (location-costmap::resolution cm-1)
                        :height-map (make-instance 'fixed-height-map))))
    (register-cost-function map (lambda (x y)
                                  (1- (location-costmap::get-grid-cell cm-1 x y))) 'free-space)
    map))

;; get costmap needs to return a complex location-costmap object
;; this object is a 2d map with costs, functions to calculate those costs,
;; functions to generate samples, and height information
(defun get-close-to-costmap (location)
  "a costmap returning non-zero values for points on the ground which are free for robot and close to location"

  ;; we create a costmap by reading the occupancy grid (for now stored in a fluent)
  ;; and by adding to that a bitmap function which is a range a round a point
  ;; The default sample generator iterates once over most of the map to find a
  ;; sample, this is not very efficient, but okay for a start
  (let* ((pose  (reference location))
         (msg (value *map-fl*))
         (padding 0.4)
         (invert nil);; inverting map in cost function, as occupancy-grid-msg->occupancy-grid cannot handle invert+padding
         (grid (occupancy-grid-msg->occupancy-grid msg :padding padding :invert invert))
         (map-locations (grid->costmap grid)))
    (register-cost-function map-locations (make-range-cost-function (cl-tf:origin pose) 0.8) 'close-space)
    map-locations))


;; costmaps for resolution of designators
(def-fact-group adapto-location-costmap (desig-costmap)

  ;; (close-to ?loc)
 (<- (desig-costmap ?desig ?cm)
    (desig-prop ?desig (close-to ?loc))
    (lisp-fun get-close-to-costmap ?loc ?cm)))


