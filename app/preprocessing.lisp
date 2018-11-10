(cl:in-package :vinoyaku.app)


(declaim (ftype (function (opticl-core:8-bit-gray-image) (values single-float))
                mid-luminosity))
(defun mid-luminosity (image)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (flet ((next-average (current-average v n)
           (declare (type single-float current-average)
                    (type (unsigned-byte 8) v)
                    (type fixnum n))
           (the single-float (+ current-average (/ (- v current-average) (1+ n))))))
    (let ((pixel-num 0)
          (average 0f0))
      (declare (type fixnum pixel-num)
               (type single-float average))
      (opticl:do-pixels (i j) image
        (multiple-value-bind (grey) (opticl:pixel image i j)
          (setf average (next-average average grey pixel-num)))
        (incf pixel-num))
      average)))


(defun preprocess-image (image)
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (let* ((transform (opticl:make-affine-transformation :x-scale 4.0 :y-scale 4.0))
         (grayscale (opticl:transform-image
                     (opticl:sharpen-image
                      (opticl:coerce-image image 'opticl-core:8-bit-gray-image))
                     transform :interpolate :bilinear)))
    (declare (type opticl-core:8-bit-gray-image grayscale))
    (let ((dark-background-p (< (mid-luminosity grayscale) 128)))
      (opticl:do-pixels (i j) grayscale
        (let* ((raw-lum (opticl:pixel grayscale i j))
               (lum (if dark-background-p (- 255 raw-lum) raw-lum)))
          (setf (opticl:pixel grayscale i j) (if (> lum 128) 255 0)))))
    grayscale))
