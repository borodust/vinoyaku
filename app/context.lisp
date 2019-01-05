(cl:in-package :vinoyaku.app)


(defstruct selected-region
  monitor
  origin
  width
  height)


(defclass application-context ()
  ((vinoyaku :initform nil)
   (selected-region :initform nil :reader selected-region-of)
   (image :initform nil :accessor region-image-of)
   (grayscale :initform nil :accessor grayscale-image-of)
   (histo :initform nil :accessor histo-of)))


(defmethod initialize-instance :after ((this application-context) &key)
  (with-slots (vinoyaku) this
    (setf vinoyaku (vinoyaku:make-context))))


(defun make-application-context ()
  (make-instance 'application-context))


(defun destroy-application-context (context)
  (with-slots (vinoyaku) context
    (vinoyaku:destroy-context vinoyaku)))


(defun update-selected-region (context monitor origin width height)
  (with-slots (selected-region) context
    (setf selected-region (make-selected-region :monitor monitor
                                                :origin origin
                                                :width width
                                                :height height))))


(defun read-selected-region (ctx scale)
  (with-slots (selected-region image grayscale histo) ctx
    (when selected-region
      (let ((origin (mult (selected-region-origin selected-region) scale))
            (width (floor (* (selected-region-width selected-region) scale)))
            (height (floor (* (selected-region-height selected-region) scale))))
        (flet ((same-bounds ()
                 (when image
                   (opticl:with-image-bounds (current-height current-width) image
                     (and (= width current-width) (= current-height height))))))
          (unless (same-bounds)
            (setf image (opticl:make-8-bit-rgba-image height width))))
        (bodge-host:read-screen-region (x origin) (y origin) width height image)
        (setf grayscale (prepare-image image)
              histo (analyze-image grayscale))))))


(defun explain (ctx image)
  (with-slots (vinoyaku) ctx
    (when image
      (opticl:with-image-bounds (height width) image
        (log:info "Explanation:~&~A" (multiple-value-list
                                      (vinoyaku:explain vinoyaku image width height)))))))
