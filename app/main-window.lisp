(cl:in-package :vinoyaku.app)

(defvar *window-width* 800)
(defvar *window-height* 600)

(defclass main-window (ui-window) ()
  (:default-initargs
   :title "びの訳"
   :width *window-width*
   :height *window-height*))


(defmethod initialize-instance ((this main-window) &rest initargs &key &allow-other-keys)
  (apply #'call-next-method this :application-context (make-application-context) initargs))


(defmethod bodge-host:on-hide ((this main-window))
  (bodge-host:close-window this))


(defmethod bodge-host:on-destroy ((this main-window))
  (destroy-application-context (application-context-of this)))


(defmethod on-rendering-context-ready ((this main-window))
  (add-window-panel this 'control-panel :context (application-context-of this)))


(defmethod on-draw ((this main-window))
  (gl:clear-color 0.2 0.2 0.2 1)
  (gl:clear :color-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass control-panel-state ()
  ((context :initarg :context :reader application-context-of)))


(defun open-selection-window (win)
  (bodge-host:open-window (make-instance 'selection-window
                                         :application-context (application-context-of win)
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
