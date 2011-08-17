(in-package :ad-exe)

;; Expectations-instances in the global structure are to be validated by the expectation validation module
(create-global-structure :expectation)



;; ;; Create instance of expectation

;; get x-value of a posestamped: (tf:x (tf:origin p1))

;; (addgv :expectation 'human-pose (make-instance 'position-expectation
;;                                   :pose (tf:make-pose-stamped
;;                                          "map"
;;                                          0.0
;;                                          (tf:make-3d-vector 0 0 0)
;;                                          (tf:make-quaternion 0 0 0 1))
;;                                   :radius 5))