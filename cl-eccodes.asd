;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2017-11-01 15:45:14>

(defsystem "cl-eccodes"
  :description "GRIB API bindings"
  :default-component-class cl-source-file.cl
  :depends-on ("cl-utilities" "zlib" "log2")
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "api")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

