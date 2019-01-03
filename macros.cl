;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2019-01-02 16:07:11>

(in-package :cl-eccodes)

(defmacro with-handle-from-index ((handlevar index) &body forms)
  (let ((resultvar (gensym "RESULT-")))
    `(let* ((,handlevar (codes-handle-new-from-index ,index))
            (,resultvar
             (multiple-value-list
              (progn ,@forms))))
       (codes-handle-delete ,handlevar)
       (values-list ,resultvar))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
