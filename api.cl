;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description  libeccodes bindings
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2017-11-01 17:03:42>

(in-package :cl-eccodes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; GRIB_API bindings
;;;  https://software.ecmwf.int/wiki/display/GRIB/GRIB+API+installation
;;;  Install with ./configure --disable-jpeg

(define-foreign-library libeccodes
  (:linux "libeccodes.so"))

(use-foreign-library libeccodes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Handles

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; codes_count_in_file

(defun codes-count-in-file (c-file)
  "Counts the messages contained in a file resource."
  (with-foreign-object
      (n '(:pointer :int))
    (let ((err
           (codes_count_in_file (null-pointer) c-file n)))
      (case err
        (0
         (mem-ref n :int))
        (otherwise
         (error "Error ~a" err))))))

(defcfun codes_count_in_file :int
  (context :pointer)
  (file :pointer)
  (n (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; codes_handle_new_from_file

(defun codes-handle-new-from-file (c-file)
  (with-foreign-object (err :int)
    (values (codes_handle_new_from_file (null-pointer) c-file err)
            (mem-ref err :int))))

(defcfun codes_handle_new_from_file
    :pointer
  (context :pointer)
  (file :pointer)
  (err (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get next selected message handle in index.
;; Function is listed in the index module, not in the handle module.
;; https://software.ecmwf.int/wiki/display/GRIB/index.c

(defun codes-handle-new-from-index (index)
  (with-foreign-object (err :int)
    (let ((handle (codes_handle_new_from_index index err)))
      (if (= 0 (mem-ref err :int))
          handle
          (error "Failed to create handle from index ~a" index)))))

(defcfun codes_handle_new_from_index
    :pointer
  (index :pointer)
  (err (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; codes_handle_delete

(defun codes-handle-delete (handle)
  (codes_handle_delete handle))

(defcfun codes_handle_delete :int (handle :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indexes
;;;
;;; The codes_index is the structure giving indexed access to messages in a file.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; codes_index_new

(defun codes-index-new (keys)
  (with-foreign-object (err :int)
    (let ((keys-string (format () "~{~a~^,~}" keys)))
      (values (codes_index_new (null-pointer)
                              keys-string
                              err)
              (mem-ref err :int)))))

(defcfun codes_index_new
    :pointer
  (context :pointer)
  (keys :string)
  (error (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; codes_index_new_from_file

(defun codes-index-new-from-file (filename keys)
  (with-foreign-object (err :int)
    (let* ((keys-string (format () "~{~a~^,~}" keys))
           (index (codes_index_new_from_file (null-pointer)
                                            filename
                                            keys-string
                                            err)))
      (if (= 0 (mem-ref err :int))
          index
          (error "Failed to open ~a, error: ~a" filename (mem-ref err :int))))))

(defcfun codes_index_new_from_file
    :pointer
  (context :pointer)
  (filename :string)
  (keys :string)
  (error (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Delerte index

(defcfun codes-index-delete :void (index :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; add_file

(defcfun codes-index-add-file
    :int
  (index :pointer)
  (filename :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; size of index wrt. key

(defun codes-index-get-size (index key)
  (with-foreign-object (size :int)
    (let ((retval (codes_index_get_size index key size)))
      (if (= retval 0)
          (mem-ref size :int)
          (values nil retval)))))

(defcfun codes_index_get_size
    :int
  (index :pointer)
  (key :string)
  (size :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get distinct values of key - long

(defun codes-index-get-long (index key
                            &optional (count (codes-index-get-size index key)))
  (with-foreign-objects
      ((values :long count)
       (size :int))
    (setf (mem-ref size :int) count)
    (let* ((res (codes_index_get_long index key values size))
           (keys (make-array count :element-type 'integer)))
      (case res
        (0
         (dotimes (k count keys)
           (setf (aref keys k) (mem-aref values :long k))))
        (otherwise
         (error "codes-index-get-long: ~a" res))))))

(defcfun codes_index_get_long
    :int
  (index :pointer)
  (key :string)
  (value :pointer)
  (size (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get distinct values of key - double

(defun codes-index-get-double (index key
                            &optional (count (codes-index-get-size index key)))
  (with-foreign-objects
      ((values :double count)
       (size :int))
    (setf (mem-ref size :int) count)
    (let ((res (codes_index_get_double index key values size))
          (keys (make-array count :element-type 'double-float)))
    (case res
      (0
       (dotimes (k count keys)
         (setf (aref keys k) (mem-aref values :double k))))
      (otherwise
       (error "codes-index-get-double: ~a" res))))))

(defcfun codes_index_get_double
    :int
  (index :pointer)
  (key :string)
  (value :pointer)
  (size (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get distinct values of key - string

(defun codes-index-get-string (index key
                            &optional (count (codes-index-get-size index key)))
  (with-foreign-objects
      ((values :pointer count)
       (size :int count))
    (setf (mem-ref size :int) count)
    (let ((res (codes_index_get_string index key values size))
          (keys (make-array count :element-type 'string)))
      (case res
        (0
         (dotimes (k count keys)
           (setf (aref keys k) (mem-aref values :string k))))
        (otherwise
         (error "codes-index-get-string: ~a" res))))))

(defcfun codes_index_get_string
    :int
  (index :pointer)
  (key :string)
  (value :pointer)
  (size (:pointer :int)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Select handles (messages) matching key

(defcfun codes-index-select-long
  :int
  (index :pointer)
  (key :string)
  (value :long))

(defcfun codes-index-select-double
  :int
  (index :pointer)
  (key :string)
  (value :double))

(defcfun codes-index-select-string
  :int
  (index :pointer)
  (key :string)
  (value :string))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Iterators

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; new

(defun codes-iterator-new (handle)
  (with-foreign-object (err '(:pointer :int)) 
    (values 
     (codes_iterator_new handle 0 err)
     (mem-ref err :int))))

(defcfun codes_iterator_new
  :pointer
  (handle :pointer)
  (flags :long)
  (err :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; next

(defun codes-iterator-next (iterator)
  (with-foreign-objects
      ((lat '(:pointer :double))
       (lon '(:pointer :double))
       (val '(:pointer :double)))
    (let ((res (codes_iterator_next iterator lat lon val)))
      (when (> res 0)
        (values (mem-ref lat :double)
                (mem-ref lon :double)
                (mem-ref val :double))))))

(defcfun codes_iterator_next :int
  (iterator :pointer)
  (lat :pointer)
  (lon :pointer)
  (val :pointer))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; previous

(defun codes-iterator-previous (iterator)
  (with-foreign-objects
      ((lat '(:pointer :double))
       (lon '(:pointer :double))
       (val '(:pointer :double)))
    (let ((res (codes_iterator_previous iterator lat lon val)))
      (when (> res 0)
        (values (mem-ref lat :double)
                (mem-ref lon :double)
                (mem-ref val :double))))))

(defcfun codes_iterator_previous :int
  (iterator :pointer)
  (lat :pointer)
  (lon :pointer)
  (val :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; reset

(defcfun codes-iterator-reset :int
  (iterator :pointer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Get Data

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get the number of coded value from a key, if several keys of the same name are present, the total sum is returned. 

(defun codes-get-size (handle key)
  (with-foreign-object
      (value :long)
    (let ((retval (codes_get_size handle key value)))
      (if (= retval 0)
          (mem-ref value :long)
          (values nil retval)))))

(defcfun codes_get_size :int
  (handle :pointer)
  (key :string)
  (value (:pointer :long)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get raw bytes values from a key. 

(defun codes-get-bytes (handle key num)
  (with-foreign-objects
      ((value :int)
       (bytes :int8 num))
    (let ((retval (codes_get_bytes handle key value num)))
      (if (= retval 0)
          (loop :for k :below num :collect (mem-aref bytes :int8))
          (error "Failed to read ~a, error ~a" key retval)))))

(defcfun codes_get_bytes :int
  (handle :pointer)
  (key :string)
  (bytes :pointer)
  (length :int))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get a long value from a key, if several keys of the same name are present, the last one is returned. 

(defun codes-get-long (handle key)
  (with-foreign-object
      (value :long)
    (let ((retval (codes_get_long handle key value)))
      (if (= retval 0)
          (mem-ref value :long)
          (error "Failed to read ~a, error ~a" key retval)))))

(defcfun codes_get_long :int
  (handle :pointer)
  (key :string)
  (value (:pointer :long)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get a double value from a key, if several keys of the same name are present, the last one is returned. 

(defun codes-get-double (handle key)
  (with-foreign-object
      (value :double)
    (let ((retval (codes_get_double handle key value)))
      (if (= retval 0)
          (mem-ref value :double)
          (values nil retval)))))

(defcfun codes_get_double :int
  (handle :pointer)
  (key :string)
  (value (:pointer :double)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get a string value from a key, if several keys of the same name are present, the last one is returned. 

(defun codes-get-string (handle key)
  (with-foreign-objects
      ((value :char 100)
       (length :int))
    (setf (mem-ref length :int) 100)
    (let ((retval (codes_get_string handle key value length)))
      (if (= retval 0)
          (foreign-string-to-lisp value)
          (error "Failed to get string value: ~a" retval)))))

(defcfun codes_get_string :int
  (handle :pointer)
  (key :string)
  (value (:pointer :string))
  (length (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get double array values from a key. 

(defun codes-get-double-array (handle key &aux (length (codes-get-size handle key)))
  ;; Try to work around bugs.launchpad.net/sbcl/+bug/1446962
  #+sbcl (sb-ext:gc :full t)
  (with-foreign-objects
      ((value :double length)
       (size :int))
    (setf (mem-aref size :int) length)
    (let ((retval
           (codes_get_double_array handle key value size)))
      (if (= retval 0)
          (let* ((length-out (mem-ref size :int))
                 (result (make-array length-out :element-type 'double-float)))
            (log2:trace "Creating array(~a)" length-out)
            (dotimes (k length-out result)
              (setf (aref result k) (mem-aref value :double k))))
          (values nil retval)))))

(defun codes-get-double-array-as-single (handle key &aux (length (codes-get-size handle key)))
  ;; Try to work around bugs.launchpad.net/sbcl/+bug/1446962
  #+sbcl (sb-ext:gc :full t)
  (with-foreign-objects
      ((value :double length)
       (size :int))
    (setf (mem-aref size :int) length)
    (let ((retval
           (codes_get_double_array handle key value size)))
      (if (= retval 0)
          (let* ((length-out (mem-ref size :int))
                 (result (make-array length-out :element-type 'single-float)))
            (log2:trace "Creating array(~a)" length-out)
            (dotimes (k length-out result)
              (setf (aref result k) (coerce (mem-aref value :double k) 'single-float))))
          (values nil retval)))))
  
(defcfun codes_get_double_array :int
  (handle :pointer)
  (key :string)
  (value :pointer)
  (length (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Get long array values from a key. 

(defun codes-get-long-array (handle key &aux (length (codes-get-size handle key)))
  ;; Try to work around bugs.launchpad.net/sbcl/+bug/1446962
  #+sbcl (sb-ext:gc :full t)
  (with-foreign-objects
      ((value :long length)
       (size :int))
    (setf (mem-aref size :int) length)
    (let ((retval
           (codes_get_long_array handle key value size)))
      (if (= retval 0)
          (let* ((length-out (mem-ref size :int))
                 (result (make-array length-out :element-type '(signed-byte 64))))
            (dotimes (k length-out result)
              (setf (aref result k) (mem-aref value :long k))))
          (values nil retval)))))

(defcfun codes_get_long_array :int
  (handle :pointer)
  (key :string)
  (value :pointer)
  (length (:pointer :int)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Additional bindings: C file access 

(defmacro with-c-file ((var path mode) &body forms)
  `(let ((,var (fopen ,path ,mode)))
     (unwind-protect
          (progn ,@forms)
       (unless (null-pointer-p ,var)
         (fclose ,var)))))

(defcfun fopen :pointer (fname :string) (mode :string))

(defcfun fclose :int (file :pointer))

(defcfun fprintf :int (file :pointer) (format :string) &rest)

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
