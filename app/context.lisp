(cl:in-package :vinoyaku.app)


(defstruct selected-region
  monitor
  origin
  width
  height)


(defclass application-context ()
  ((vinoyaku :initform nil)
   (selected-region :initform nil)))


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



(defun read-selected-region-into-rgba-image (context)
  (with-slots (vinoyaku selected-region) context
    (when selected-region
      (let* ((origin (selected-region-origin selected-region))
             (width (floor (selected-region-width selected-region)))
             (height (floor (selected-region-height selected-region)))
             (image (opticl:make-8-bit-rgba-image height width)))
        (bodge-util:with-simple-array-pointer (ptr image)
          (bodge-host:read-screen-region (floor (x origin)) (floor (y origin))
                                         width height ptr))
        (log:info "Explanation:~&~A" (multiple-value-list
                                      (vinoyaku:explain vinoyaku image width height)))
        image))))
