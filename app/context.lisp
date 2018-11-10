(cl:in-package :vinoyaku.app)


(defstruct selected-region
  monitor
  origin
  width
  height)


(defclass application-context ()
  ((vinoyaku :initform nil)
   (selected-region :initform nil :reader selected-region-of)
   (image :initform nil :accessor region-image-of)))


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


(defun read-selected-region (ctx)
  (with-slots (selected-region image) ctx
    (when selected-region
      (let ((origin (selected-region-origin selected-region))
             (width (floor (selected-region-width selected-region)))
             (height (floor (selected-region-height selected-region))))
        (flet ((same-bounds ()
                 (when image
                   (opticl:with-image-bounds (current-height current-width) image
                     (and (= width current-width) (= current-height height))))))
          (unless (same-bounds)
            (setf image (opticl:make-8-bit-rgba-image height width))))
        (bodge-util:with-simple-array-pointer (ptr image)
          (bodge-host:read-screen-region (floor (x origin)) (floor (y origin))
                                         width height ptr))))))


(defun explain-context-image (ctx)
  (with-slots (vinoyaku image) ctx
    (when image
      (let ((preprocessed (preprocess-image image)))
        (opticl:with-image-bounds (height width) preprocessed
          (log:info "Explanation:~&~A" (multiple-value-list
                                        (vinoyaku:explain vinoyaku preprocessed width height))))))))
