(cl:in-package :vinoyaku.app)


(defun prepare-image (image)
  (opticl:coerce-image image 'opticl-core:8-bit-gray-image))


(defun analyze-image (grayscale)
  (let ((histo (make-array 256 :element-type 'fixnum :initial-element 0)))
    (opticl:do-pixels (i j) grayscale
      (incf (aref histo (opticl:pixel grayscale i j))))
    histo))


(defun make-foreground-mask (histo threshold from to &optional invert)
  (let* ((mask (make-array (length histo) :element-type 'boolean :initial-element invert))
         (max-value (* (reduce #'max histo) threshold)))
    (loop for i from from upto (or to (1- (length histo)))
          when (>= (aref histo i) max-value)
            do (setf (aref mask i) (not invert)))
    mask))


(defun preprocess-image (grayscale foreground-mask)
  (declare #++(optimize (speed 3) (safety 0) (debug 0)))
  (let* ((transform (opticl:make-affine-transformation :x-scale 4.0 :y-scale 4.0))
         (transformed (opticl:transform-image grayscale transform :interpolate :bilinear)))
    (opticl:do-pixels (i j) transformed
      (let* ((lum (opticl:pixel transformed i j)))
        (setf (opticl:pixel transformed i j) (if (aref foreground-mask lum) 0 255))))
    transformed))
