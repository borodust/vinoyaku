(cl:in-package :vinoyaku.app)

(defvar *window-width* 800)
(defvar *window-height* 600)

(defclass main-window (ui-window)
  ((vinoyaku-context :initform nil :reader context-of))
  (:default-initargs
   :title "びの訳"
   :width *window-width*
   :height *window-height*))


(defmethod initialize-instance :after ((this main-window) &key)
  (with-slots (vinoyaku-context) this
    (setf vinoyaku-context (vinoyaku:make-context))))


(defmethod bodge-host:on-destroy ((this main-window))
  (with-slots (vinoyaku-context) this
    (vinoyaku:destroy-context vinoyaku-context)
    (setf vinoyaku-context nil)))


(defmethod on-rendering-context-ready ((this main-window))
  (with-slots (vinoyaku-context) this
    (add-window-panel this 'control-panel :context vinoyaku-context)))


(defmethod on-draw ((this main-window))
  (gl:clear-color 0.2 0.2 0.2 1)
  (gl:clear :color-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass control-panel-state ()
  ((context :initarg :context :reader context-of)))


(defun open-selection-window (win)
  (bodge-host:open-window (make-instance 'selection-window
                                         :context (context-of win)
                                         :transparent t
                                         :decorated nil
                                         :resizable t
                                         :floating t)))


(bodge-ui:defwindow (control-panel
                     (:title "YO")
                     (:origin 50 100)
                     (:width 400) (:height 72)
                     (:options :movable)
                     (:inherit control-panel-state))
  (bodge-ui:button :label "HI" :on-click #'open-selection-window))
