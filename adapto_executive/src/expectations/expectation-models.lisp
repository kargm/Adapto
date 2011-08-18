(in-package :ad-exe)

;; Here we define the instances of our expectations and put them into a global structure
;; For example here: An expectation about the human beeing no more than 6 meters away from Jido
(defun generate-expectations ()
  (create-global-structure :expectations)
  (addgv :expectations 'louis-near-jido (make-instance 'position-expectation
                                    :area (make-instance 'circle
                                            :radius 5
                                            :x (tf:x (tf:origin (pose (value (getgv :robot 'jido)))))
                                            :y (tf:y (tf:origin (pose (value (getgv :robot 'jido))))))
                                    :pose (pose (value (getgv :human 'louis))))))