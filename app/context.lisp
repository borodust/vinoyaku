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
