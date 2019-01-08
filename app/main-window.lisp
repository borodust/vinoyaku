(cl:in-package :vinoyaku.app)

(defparameter *window-width* (* 800 #++ 2.6))
(defparameter *window-height* (* 600 #++ 2.6))

(defclass main-window (ui-window)
  ((scan-paint :initform nil :accessor scan-paint-of))
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
  (bodge-canvas:clear-buffers (vec4 0.2 0.2 0.2 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass main-panel-model ()
  ((window :initarg :window :reader window-of)))


(defmethod root-window ()
  (window-of (bodge-ui:root-panel)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun explain-it (panel)
  (declare (ignore panel))
  (explain (application-context-of (root-window))))


(defun open-selection-window (panel)
  (declare (ignore panel))
  (let ((win (root-window)))
    (bodge-host:progm
      (let* ((monitor (bodge-host:window-monitor win))
             (video-mode (bodge-host:monitor-video-mode monitor)))
        (bodge-host:open-window (make-instance 'selection-window
                                               :application-context (application-context-of win)
                                               :position (vec2 0 0)
                                               :width (bodge-host:video-mode-width video-mode)
                                               :height (bodge-host:video-mode-height video-mode)))))))


(defun update-peephole (panel threshold from to &optional invert)
  (let* ((win (window-of panel))
         (ctx (application-context-of win))
         (mask (make-foreground-mask (histo-of ctx) threshold from to invert))
         (preprocessed (preprocess-image (grayscale-image-of ctx) mask)))
    (let ((preprocessed-rgba (opticl:coerce-image preprocessed
                                                  'opticl-core:8-bit-rgba-image))
          (canvas (bodge-ui-window:ui-window-canvas win))
          (scan-paint (scan-paint-of win)))
      (opticl:with-image-bounds (height width) preprocessed-rgba
        (when scan-paint
          (bodge-canvas:destroy-image-paint canvas scan-paint))
        (setf (scan-paint-of win) (bodge-canvas:make-rgba-image-paint canvas
                                                                      preprocessed-rgba
                                                                      width height
                                                                      :flip-vertically t)
              (processed-image-of ctx) preprocessed-rgba)))))


(defun scan-selected-region (panel)
  (let* ((histogram-widget (bodge-ui:find-element :histogram panel))
         (win (window-of panel))
         (ctx (application-context-of win)))
    (multiple-value-bind (bound-start bound-end) (histogram-bounds histogram-widget)
      (bodge-host:progm
        (read-selected-region ctx (bodge-host:viewport-scale win))
        (alexandria:when-let* ((grayscale (grayscale-image-of ctx)))
          (bodge-ui-window:within-ui-thread (win)
            (update-histogram-array histogram-widget (histo-of ctx))
            (update-peephole panel
                             (histogram-threshold histogram-widget)
                             bound-start bound-end)))))))


(defun update-preview (panel threshold from to)
  (let* ((group (bodge-ui:find-element :ground-switch))
         (active-ground (bodge-ui:name-of (bodge-ui:active-radio-button-of group))))
    (update-peephole panel threshold from to (eq active-ground :background))))


(defun switch-background (panel active-button)
  (let* ((histogram-widget (bodge-ui:find-element :histogram panel))
         (win (window-of panel))
         (ctx (application-context-of win)))
    (alexandria:when-let ((grayscale (grayscale-image-of ctx)))
      (multiple-value-bind (bound-start bound-end) (histogram-bounds histogram-widget)
        (update-peephole panel
                         (histogram-threshold histogram-widget)
                         bound-start bound-end (and active-button
                                                    (eq (bodge-ui:name-of active-button) :background)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass peephole (bodge-ui:custom-widget) ())


(defmethod bodge-ui:custom-widget-height ((this peephole))
  (alexandria:when-let ((image (region-image-of (application-context-of (root-window)))))
    (opticl:with-image-bounds (height width) image
      (declare (ignore width))
      height)))


(defun draw-placeholder-rect (origin width height)
  (let* ((amp 0.2)
         (cos (+ (* (cos (bodge-util:real-time-seconds)) amp) 0.5))
         (sin (+ (* (sin (bodge-util:real-time-seconds)) amp) 0.5))
         (r sin)
         (g cos)
         (b (/ sin cos)))
    (bodge-canvas:draw-rect origin width height
                            :fill-paint (vec4 r g b 1)
                            :rounding 4)))

(defmethod bodge-ui:render-custom-widget ((this peephole) origin width height)
  (alexandria:if-let ((image (region-image-of (application-context-of (root-window)))))
    (opticl:with-image-bounds (height width) image
      (bodge-canvas:draw-image origin width height (scan-paint-of (root-window))
                               :rounding 4
                               :scale-x 1/4
                               :scale-y 1/4))
    (draw-placeholder-rect origin width height)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(bodge-ui:defpanel (control-panel
                    (:origin 50 50)
                    (:width 700) (:height 500)
                    (:options :headerless)
                    (:inherit main-panel-model))
  (bodge-ui:horizontal-layout
   (bodge-ui:button :label "Explain" :on-click #'explain-it)
   (bodge-ui:button :label "Scan" :on-click #'scan-selected-region)
   (bodge-ui:button :label "Select" :on-click #'open-selection-window)
   (bodge-ui:button :label "Options"))
  (bodge-ui:radio-group
   :name :ground-switch :on-change #'switch-background
   (bodge-ui:horizontal-layout
    (bodge-ui:radio :name :foreground :label "Foreground" :activated t)
    (bodge-ui:radio :name :background :label "Background")))
  (histogram :name :histogram :on-parameter-change #'update-preview)
  (peephole))
