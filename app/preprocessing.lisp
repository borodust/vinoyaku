(cl:in-package :vinoyaku.app)



(defun preprocess-image (image)
  (declare #++(optimize (speed 3) (safety 0) (debug 0)))
  (let* ((grayscale (opticl:coerce-image image 'opticl-core:8-bit-gray-image))
         (transform (opticl:make-affine-transformation :x-scale 4.0 :y-scale 4.0))
         (transformed (opticl:transform-image grayscale transform :interpolate :bilinear))
         (histo (make-array 256 :element-type 'fixnum :initial-element 0)))
    (opticl:do-pixels (i j) transformed
      (let* ((lum (opticl:pixel transformed i j)))
        (setf (opticl:pixel transformed i j) (if (< 128 lum) 0 255))))
    (opticl:do-pixels (i j) grayscale
      (incf (aref histo (opticl:pixel grayscale i j))))
    (values transformed histo)))
