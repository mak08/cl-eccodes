;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-11-01 17:13:19>

(in-package :cl-eccodes)

(defmacro with-handle-from-index ((handlevar index) &body forms)
  (let ((resultvar (gensym "RESULT-")))
    `(let* ((,handlevar (codes-handle-new-from-index ,index))
            (,resultvar
             (progn ,@forms)))
       (codes-handle-delete ,handlevar)
       ,resultvar)))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
