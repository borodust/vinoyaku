(cl:in-package :vinoyaku.app)


(defclass ui-window (bodge-ui-window:ui-window)
  ((application-context :initarg :application-context :reader application-context-of
                        :initform (error ":application-context missing")))
  (:default-initargs :opengl-version '(2 1)))
