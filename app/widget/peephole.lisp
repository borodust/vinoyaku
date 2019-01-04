(cl:in-package :vinoyaku.app)


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
        (bodge-canvas:draw-image origin width height (scan-paint-of (root-window))
                                 :rounding 4
                                 :scale-x 1/4
                                 :scale-y 1/4))
      (bodge-canvas:draw-rect origin width height
                              :fill-paint (vec4 r g b 1)
                              :rounding 4))))
