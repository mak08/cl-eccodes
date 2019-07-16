;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2019-07-15 23:23:44>

(defpackage "CL-ECCODES"
  (:use "COMMON-LISP" "LOCAL-TIME" "CFFI")
  (:export  "WITH-C-FILE"
            "FOPEN"
            "FCLOSE"

            "CODES-GET-ERROR-MESSAGE"
            
            "CODES-COUNT-IN-FILE"

            "CODES-INDEX-NEW-FROM-FILE"
            "CODES-INDEX-NEW"
            "CODES-INDEX-DELETE"
            "CODES-INDEX-ADD-FILE"

            "CODES-INDEX-GET-SIZE"

            "CODES-INDEX-GET-LONG"
            "CODES-INDEX-GET-DOUBLE"
            "CODES-INDEX-GET-STRING"

            "CODES-INDEX-SELECT-LONG"
            "CODES-INDEX-SELECT-DOUBLE"
            "CODES-INDEX-SELECT-STRING"

            "CODES-HANDLE-NEW-FROM-FILE"
            "WITH-HANDLE-FROM-INDEX"
            "CODES-HANDLE-NEW-FROM-INDEX"
            "CODES-HANDLE-DELETE"

            "CODES-GET-SIZE"

            "CODES-GET-LONG"
            "CODES-GET-DOUBLE"
            "CODES-GET-BYTES"
            "CODES-GET-STRING"
            "CODES-GET-LONG-ARRAY"
            "CODES-GET-DOUBLE-ARRAY"
            "CODES-GET-DOUBLE-ARRAY-AS-SINGLE"

            "CODES-GRIB-ITERATOR-NEW"
            "CODES-GRIB-ITERATOR-NEXT"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
