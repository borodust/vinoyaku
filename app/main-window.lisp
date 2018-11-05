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


(defun scan-selected-region (win)
  (read-selected-region-into-rgba-image (application-context-of win)))


(defclass peephole (bodge-ui:custom-widget) ())


(defmethod bodge-ui:custom-widget-height ((this peephole))
  458)


(defmethod bodge-ui:render-custom-widget ((this peephole) origin width height)
  (bodge-canvas:draw-rect origin width height
                          :fill-paint (vec4 (+ (* (sin (bodge-util:real-time-seconds)) 0.4) 0.5)
                                            (+ (* (cos (bodge-util:real-time-seconds)) 0.4) 0.5)
                                            (+ (* (cos (bodge-util:real-time-seconds)) 0.4) 0.5)
                                            1) :rounding 4))


(bodge-ui:defwindow (control-panel
                     (:origin 50 50)
                     (:width 700) (:height 500)
                     (:options :headerless)
                     (:inherit control-panel-state))
  (bodge-ui:horizontal-layout
   (bodge-ui:button :label "Scan" :on-click #'scan-selected-region)
   (bodge-ui:button :label "Select" :on-click #'open-selection-window)
   (bodge-ui:button :label "Options"))
  (peephole))
