;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2025-12-30 22:20:51>

(in-package :cl-eccodes)

(defmacro with-handle-from-index ((handlevar index) &body forms)
  (let ((resultvar (gensym "RESULT-")))
    `(let* ((,handlevar (codes-handle-new-from-index ,index))
            (,resultvar
             (multiple-value-list
              (progn ,@forms))))
       (codes-handle-delete ,handlevar)
       (values-list ,resultvar))))

(defmacro with-grib-index ((handlevar variables) &body forms)
  (let ((resultvar (gensym "RESULT-")))
    `(let* ((,handlevar (codes-index-new ,variables)))
       (unwind-protect
            (let ((,resultvar
                    (multiple-value-list
                     (progn ,@forms))))
              (values-list ,resultvar))
         (codes-index-delete ,handlevar)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
