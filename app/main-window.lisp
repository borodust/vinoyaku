(cl:in-package :vinoyaku.app)

(defvar *window-width* 800)
(defvar *window-height* 600)

(defclass main-window (ui-window)
  ((scan-paint :initform nil :reader scan-paint-of))
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

(defun open-selection-window (panel)
  (declare (ignore panel))
  (bodge-host:open-window (make-instance 'selection-window
                                         :application-context (application-context-of *window*)
                                         :transparent t
                                         :decorated nil
                                         :resizable t
                                         :floating t)))


(defun scan-selected-region (panel)
  (declare (ignore panel))
  (with-slots (scan-paint) *window*
    (let* ((win *window*)
           (ctx (application-context-of win)))
      (bodge-host:progm
        (read-selected-region ctx)
        (within-rendering-thread (win)
          (let ((image (region-image-of ctx)))
            (opticl:with-image-bounds (height width) image
              (when scan-paint
                (bodge-canvas:destroy-image-paint (canvas-of win) scan-paint))
              (setf scan-paint (bodge-canvas:make-rgba-image-paint (canvas-of win)
                                                                   image width height)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass peephole (bodge-ui:custom-widget) ())


(defmethod bodge-ui:custom-widget-height ((this peephole))
  458)


(defmethod bodge-ui:render-custom-widget ((this peephole) origin width height)
  (let* ((amp 0.3)
         (cos (+ (* (cos (bodge-util:real-time-seconds)) amp) 0.5))
         (sin (+ (* (sin (bodge-util:real-time-seconds)) amp) 0.5))
         (r sin)
         (g cos)
         (b (/ sin cos)))
    (bodge-canvas:draw-rect origin width height
                            :fill-paint (alexandria:if-let ((paint (scan-paint-of *window*)))
                                          paint
                                          (vec4 r g b 1))
                            :rounding 4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bodge-ui:defwindow (control-panel
                     (:origin 50 50)
                     (:width 700) (:height 500)
                     (:options :headerless))
  (bodge-ui:horizontal-layout
   (bodge-ui:button :label "Scan" :on-click #'scan-selected-region)
   (bodge-ui:button :label "Select" :on-click #'open-selection-window)
   (bodge-ui:button :label "Options"))
  (peephole))
