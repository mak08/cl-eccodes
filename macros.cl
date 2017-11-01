;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-11-01 15:44:32>

(in-package :cl-eccodes)

(defmacro with-handle-from-index ((handlevar index) &body forms)
  (let ((resultvar (gensym "RESULT-")))
    `(let* ((,handlevar (grib-handle-new-from-index ,index))
            (,resultvar
             (progn ,@forms)))
       (grib-handle-delete ,handlevar)
       ,resultvar)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
