(cl:in-package :vinoyaku)

(alexandria:define-constant +channels+ 4)


(defstruct (foreign-image
             (:constructor %make-foreign-image))
  (ptr nil :read-only t)
  (channels nil :read-only t)
  (width nil :read-only t)
  (height nil :read-only t))


(defun make-foreign-image (width height &optional (channels +channels+))
  (%make-foreign-image
   :ptr (claw:alloc :unsigned-char (* width height +channels+))
   :channels channels
   :width width
   :height height))


(defun release-foreign-image (image)
  (claw:free (foreign-image-ptr image)))


(defmacro with-foreign-image ((image width height &optional (channels +channels+)) &body body)
  `(let ((,image (make-foreign-image ,width ,height ,channels)))
     (unwind-protect
          (progn ,@body)
       (release-foreign-image ,image))))


(defun foreign-image->opticl-image (image)
  (let ((data-ptr (foreign-image-ptr image))
        (width (foreign-image-width image))
        (height (foreign-image-height image))
        (channels (foreign-image-channels image)))
    (claw:c-let ((data :unsigned-char :ptr data-ptr))
      (let ((pixel-data (make-array (list height width channels)
                                    :element-type '(unsigned-byte 8)
                                    :initial-element 0)))
        (loop for i from 0 below height
           do (loop for j from 0 below width
                 for pixel-idx = (+ (* i width) j)
                 do (loop for k from 0 below channels
                       for component-idx = (+ (* pixel-idx channels) k)
                       do (setf (aref pixel-data i j k) (data component-idx)))))
        pixel-data))))


(defun opticl-image->foreign-image (image)
  (opticl:with-image-bounds (height width channels) image
    (let* ((channels (or channels 1))
           (foreign (make-foreign-image width height channels))
           (data-ptr (foreign-image-ptr foreign)))
      (claw:c-let ((data :unsigned-char :ptr data-ptr))
        (loop for i from 0 below height
           do (loop for j from 0 below width
                 for pixel-idx = (+ (* i width) j)
                 do (if (> channels 1)
                        (loop for k from 0 below channels
                           for component-idx = (+ (* pixel-idx channels) k)
                           do (setf (data component-idx) (aref image i j k)))
                        (setf (data pixel-idx) (aref image i j))))))
      foreign)))


(declaim (ftype (function ((simple-array (unsigned-byte 8) (* *))) (values (unsigned-byte 8)))
                mid-luminosity))
(defun mid-luminosity (image)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (flet ((next-average (current-average v n)
           (declare (type double-float v current-average)
                    (type fixnum n))
           (+ current-average (/ (- v current-average) (1+ n)))))
    (let ((pixel-num 0)
          (average 0d0))
      (declare (type fixnum pixel-num))
      (opticl:do-pixels (i j) image
        (let ((lum (float (opticl:pixel image i j) 0d0)))
          (setf average (next-average average lum pixel-num)))
        (incf pixel-num))
       (values (floor average)))))



(defun preprocess-image (image)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let ((grayscale (opticl:sharpen-image
                    (opticl:coerce-image image 'opticl-core:8-bit-gray-image)))
        (transform (opticl:make-affine-transformation :x-scale 4.0 :y-scale 4.0)))
    (declare (type (simple-array (unsigned-byte 8) (* *)) grayscale))
    (let ((dark-background-p (< (mid-luminosity grayscale) 128)))
      (opticl:do-pixels (i j) grayscale
        (let* ((raw-lum (opticl:pixel grayscale i j))
               (lum (if dark-background-p (- 255 raw-lum) raw-lum)))
          (setf (opticl:pixel grayscale i j)
                (if (> lum 50) 255 lum)))))
    (opticl:transform-image grayscale transform :interpolate :bilinear)))


(defun save-image (image path)
  (opticl:write-png-file path image))


(defun ocr (image)
  (claw:with-float-traps-masked ()
    (let ((api (%tess:base-api-create)))
      (unwind-protect
           (progn
             (assert (= 0 (%tess:base-api-init3 api nil "jpn")))
             (log:debug "tesserect datapath: ~A" (%tess:base-api-get-datapath api))
             (%tess:base-api-set-variable api "chop_enable" "T")
             (%tess:base-api-set-variable api "use_new_state_cost" "F")
             (%tess:base-api-set-variable api "enable_new_segsearch" "0")
             #++(%tess:base-api-set-variable api "language_model_ngram_on" "0")
             #++(%tess:base-api-set-variable api "textord_force_make_prop_words" "F")
             #++(%tess:base-api-set-variable api "edges_max_children_per_outline" "40")
             (%tess:base-api-set-image api
                                          (foreign-image-ptr image)
                                          (foreign-image-width image)
                                          (foreign-image-height image)
                                          (foreign-image-channels image)
                                          (*  (foreign-image-channels image)
                                              (foreign-image-width image)))
             (assert (= 0 (%tess:base-api-recognize api nil)))
             (%tess:base-api-get-utf8text api))
        (%tess:base-api-end api)
        (%tess:base-api-delete api)))))


(defun recognize (image)
  (let* ((preprocessed (preprocess-image image))
         (foreign-image (opticl-image->foreign-image preprocessed)))
    #++(save-image preprocessed "/tmp/screenshot.png")
    (unwind-protect
         (ocr foreign-image)
      (release-foreign-image foreign-image))))
