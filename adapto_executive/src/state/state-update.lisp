(in-package :ad-exe)

(defun store-pose (target-pose source-pose stamp)
  (setf (tf:x (tf:origin target-pose))
        (geometry_msgs-msg:x (geometry_msgs-msg:position source-pose)))
  (setf (tf:y (tf:origin target-pose))
        (geometry_msgs-msg:y (geometry_msgs-msg:position source-pose)))
  (setf (tf:z (tf:origin target-pose))
        (geometry_msgs-msg:z (geometry_msgs-msg:position source-pose)))
  (setf (tf:x (tf:orientation target-pose))
        (geometry_msgs-msg:x (geometry_msgs-msg:orientation source-pose)))
  (setf (tf:y (tf:orientation target-pose))
        (geometry_msgs-msg:y (geometry_msgs-msg:orientation source-pose)))
  (setf (tf:z (tf:orientation target-pose))
        (geometry_msgs-msg:z (geometry_msgs-msg:orientation source-pose)))
  (setf (tf:w (tf:orientation target-pose))
        (geometry_msgs-msg:w (geometry_msgs-msg:orientation source-pose)))
  (setf (tf:stamp target-pose) stamp))

(defun store-position-data (data)
  (store-pose (pose (value (getgv :robot 'jido)))
              (geometry_msgs-msg:pose (nav_msgs-msg:pose data))
              (std_msgs-msg:stamp (nav_msgs-msg:header data))))

(defun store-human-position-data (data)
  (store-pose (pose (value (getgv :human 'louis)))
              (geometry_msgs-msg:pose data)
              (std_msgs-msg:stamp (geometry_msgs-msg:header data))))

;; Creates one object from object data
(defun create-object (obj-data)
       (destructuring-bind (name type x y z qx qy qz qw) obj-data
         
         (addgv :kitchen-object name
                (make-fluent :name name
                             :value (make-instance type
                                      :pose (tf:make-pose-stamped
                                             "map"                         ; frame-id
                                             0.0                           ; stamp TODO: get in LISP
                                             (tf:make-3d-vector x y z)     ; translation/origin
                                             (tf:make-quaternion qx qy qz qw))))))) ; rotation/orientation

;; Gets String with list of objects. Objects have format: ( Name Description x y z qx qy qz qw ) (qx, qy, qz, qw refer to objects' rotation as qaternion )
(defun create-objects (data)
   (dolist (obj-data (read-from-string data)) (create-object obj-data)))

;; Updates Object data from list of objects. Objects have format: ( Name Description x y z qx qy qz qw ) (qx, qy, qz, qw refer to objects' rotation as qaternion )
;; TODO: Check if object already exists, if not: create object
(defun store-object-data (data)

  (dolist (obj-data (read-from-string data))
    (destructuring-bind (name type x y z qx qy qz qw) obj-data
      (declare (ignore type))

      (if (isgv :kitchen-object name) 
          (let ( (target-pose (pose [getgv :kitchen-object name])) )
            (setf (tf:x (tf:origin target-pose)) x)
            (setf (tf:y (tf:origin target-pose)) y)
            (setf (tf:z (tf:origin target-pose)) z)
            (setf (tf:x (tf:orientation target-pose)) qx)
            (setf (tf:y (tf:orientation target-pose)) qy)
            (setf (tf:z (tf:orientation target-pose)) qz)
            (setf (tf:w (tf:orientation target-pose)) qw))
          (create-object obj-data)))))
        ;; (setf (tf:stamp target-pose) stamp)

;;; Objekte:
;;; - am Anfang einmal Ground Truth topic abhören (dann unsubscriben), dabei statevars für Objekte erzeugen
;;; - dann Topic mit kamera-gefilterten Daten zum Update verwenden

(let ( (topics `(("/Jido/Pose_sensor" "nav_msgs/Odometry" ,#'store-position-data)
                 ("/Human/Pose" "geometry_msgs/PoseStamped" ,#'store-human-position-data)
                 ("/Jido/Object_tracker" "std_msgs/String" ,#'store-object-data)))
       (subscribers '()) )
  (defun start-statevar-update ()
    (dolist (top topics)
      (push (apply #'roslisp:subscribe top) subscribers))
    (sleep 2))

  (defun stop-statevar-update ()
    (when subscribers
      (roslisp:unsubscribe (pop subscribers))
      (stop-statevar-update)))

)

;(setf (gethash 'start-adapto-statevar-update cram-roslisp-common::*ros-init-functions*)
;      #'start-statevar-update)
