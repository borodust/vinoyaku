(cl:in-package :vinoyaku.app)

(defparameter *window-width* (* 800 2.6))
(defparameter *window-height* (* 600 2.6))

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


(defmethod bodge-ui-window:on-ui-ready ((this main-window))
  (bodge-ui-window:add-window-panel this 'control-panel :window this))


(defmethod bodge-ui-window:on-draw ((this main-window))
  (gl:clear-color 0.2 0.2 0.2 1)
  (gl:clear :color-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass main-panel-model ()
  ((window :initarg :window :reader window-of)))


(defmethod root-window ()
  (window-of (bodge-ui:root-panel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun open-selection-window (panel)
  (declare (ignore panel))
  (bodge-host:open-window (make-instance 'selection-window
                                         :application-context (application-context-of (root-window))
                                         :transparent t
                                         :decorated nil
                                         :resizable t
                                         :floating t)))


(defun scan-selected-region (panel)
  (with-slots (scan-paint) *window*
    (let* ((histogram-widget (bodge-ui:find-element :histogram panel))
           (win *window*)
           (ctx (application-context-of win)))
      (bodge-host:progm
        (read-selected-region ctx)
        (bodge-ui-window:within-ui-thread (win)
          (alexandria:when-let* ((image (region-image-of ctx)))
            (multiple-value-bind (preprocessed histo) (preprocess-image image)
              (setf (histogram-array histogram-widget) histo)
              (let ((preprocessed (opticl:coerce-image preprocessed
                                                       'opticl-core:8-bit-rgba-image))
                    (canvas (bodge-ui-window:ui-window-canvas win)))
                (opticl:with-image-bounds (height width) preprocessed
                  (when scan-paint
                    (bodge-canvas:destroy-image-paint canvas scan-paint))
                  (setf scan-paint (bodge-canvas:make-rgba-image-paint canvas
                                                                       preprocessed width height
                                                                       :flip-vertically t)))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass peephole (bodge-ui:custom-widget) ())


(defmethod bodge-ui:custom-widget-height ((this peephole))
  (alexandria:when-let ((image (region-image-of (application-context-of (root-window)))))
    (opticl:with-image-bounds (height width) image
      (declare (ignore width))
      height)))


(defmethod bodge-ui:render-custom-widget ((this peephole) origin width height)
  (let* ((amp 0.2)
         (cos (+ (* (cos (bodge-util:real-time-seconds)) amp) 0.5))
         (sin (+ (* (sin (bodge-util:real-time-seconds)) amp) 0.5))
         (r sin)
         (g cos)
         (b (/ sin cos)))
    (alexandria:if-let ((image (region-image-of (application-context-of (root-window)))))
      (opticl:with-image-bounds (height width) image
        (bodge-canvas:draw-image origin width height (scan-paint-of *window*)
                                 :rounding 4
                                 :scale-x 1/4
                                 :scale-y 1/4))
      (bodge-canvas:draw-rect origin width height
                              :fill-paint (vec4 r g b 1)
                              :rounding 4))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bodge-ui:defpanel (control-panel
                    (:origin 50 50)
                    (:width 700) (:height 500)
                    (:options :headerless)
                    (:inherit main-panel-model))
  (bodge-ui:horizontal-layout
   (bodge-ui:button :label "Scan" :on-click #'scan-selected-region)
   (bodge-ui:button :label "Select" :on-click #'open-selection-window)
   (bodge-ui:button :label "Options"))
  (histogram :name :histogram)
  (peephole))
