;;; -*- lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert
;;; Created        22/03/2000 11:15:16
;;; Last Modified  <michael 2021-06-09 20:24:01>

(defsystem "cl-eccodes"
  :description "GRIB API bindings"
  :default-component-class cl-source-file.cl
  :depends-on ("cl-utilities" "log2" "makros" "zlib")
  :serial t
  :components ((:file "package")
               (:file "macros")
               (:file "api")))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

