(in-package :cl-user)

(desig-props:def-desig-package adapto-designator
    (:nicknames :adapto-desig)
  (:use #:cl
        #:roslisp ;; for subscribe
        #:cram-roslisp-common ;; for *tf*
        #:cram-reasoning ;; for prolog
        #:desig ;; for  base predicates
        #:location-costmap ;; for costmap predicates
        )
  (:import-from :cpl #:make-fluent #:value)
  (:desig-properties
   #:close-to #:pose))

