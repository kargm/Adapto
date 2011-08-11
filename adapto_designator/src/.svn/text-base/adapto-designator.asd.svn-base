(defsystem adapto-designator
  :depends-on (designators
               cl-tf
               cram-reasoning
               cram-language
               roslisp
               cram-roslisp-common
               nav_msgs-msg
               location-costmap)
  ;; using 2 modules as we need to use 2 lisp packages, because cram-pl and cram-reasoning (prolog) cannot both be :used together
  :components
  ((:module "adapto-desig"
            :components
            ((:file "package")
             (:file "map-handler" :depends-on ("package"))
             (:file "location-designator" :depends-on ("package" "map-handler"))
             ))))