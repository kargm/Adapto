(in-package :ad-exe)

;; Validate alle expectations in global structure
(defun validate-expectations ()
  (map-global-structure 'validate-expectation :expectations))
