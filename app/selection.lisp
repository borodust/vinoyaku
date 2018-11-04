(cl:in-package :vinoyaku.app)


(defstruct selection
  (position (vec2 100 100))
  (width 100)
  (height 100)
  (transform-mode-p nil))


(defun unpremult (value a)
  (if (= 0 a)
      value
      (floor (min (/ (* 255 value) a) 255))))


(defun read-selected-region-into-rgba-image (selection y-offset)
  (let* ((position (selection-position selection))
         (x (floor (x position)))
         (y (floor (+ (y position) y-offset)))
         (width (floor (selection-width selection)))
         (height (floor (selection-height selection)))
         (image (opticl:make-8-bit-rgba-image height width)))
    (bodge-util:with-simple-array-pointer (ptr image)
      (bodge-host:read-screen-region x y width height ptr))
    (opticl:write-png-file "/tmp/test.png" image)
    (log:info "REGION READ: ~A ~A ~A ~A" x y width height)
    image))


(defun canvas-width ()
  (with-slots (canvas) *window*
    (bodge-canvas:canvas-width canvas)))


(defun canvas-height ()
  (with-slots (canvas) *window*
    (bodge-canvas:canvas-height canvas)))


(defun draw-background (position width height)
  (bodge-canvas:path
    (bodge-canvas:rounded-rect (vec2 0 0) (canvas-width) (canvas-height))
    (bodge-canvas:rounded-rect position width height 5)
    (bodge-canvas:wind-path :hole)

    (setf (bodge-canvas:fill-paint) (vec4 0 0 0 0.7))
    (bodge-canvas:fill-path)))


(defun draw-selection-frame (position width height)
  (bodge-canvas:draw-rect position width height
                          :stroke-paint (vec4 0.5 0.7 0.7 1)
                          :thickness 3
                          :rounding 5)
  (bodge-canvas:draw-line (add position (vec2 (/ width 2) 0))
                          (add position (vec2 (/ width 2) height))
                          (vec4 0.5 0.7 0.7 0.8)
                          :thickness 1.5)
  (bodge-canvas:draw-line (add position (vec2 0 (/ height 2)))
                          (add position (vec2 width (/ height 2)))
                          (vec4 0.5 0.7 0.7 0.8)
                          :thickness 1.5))


(defun draw-transform-overlay (position width &optional (radius 6))
  (bodge-canvas:draw-circle (add position (vec2 width 0)) radius
                            :fill-paint (vec4 0.75 0.2 0.2 0.8)))


(defun draw-selection (selection)
  (let ((position (selection-position selection))
        (width (selection-width selection) )
        (height (selection-height selection)))
    (draw-background position width height)
    (draw-selection-frame position width height)
    (when (selection-transform-mode-p selection)
      (draw-transform-overlay position width))))


(defun intersecting-rect-p (rect-origin rect-width rect-height point &optional (border 0))
  (let ((x-len (- (x point) (- (x rect-origin) border)))
        (y-len (- (y point) (- (y rect-origin) border))))
    (and (< 0 x-len (+ rect-width (* 2 border)))
         (< 0 y-len (+ rect-height (* 2 border))))))


(defun intersecting-selection-p (selection cursor-position &optional (border 5))
  (intersecting-rect-p (selection-position selection)
                       (selection-width selection)
                       (selection-height selection)
                       cursor-position
                       border))


(defun intersecting-circle-p (origin radius point)
  (>= radius (vector-length (subt point origin))))


(defun intersecting-corner-p (selection cursor-position &optional (radius 10))
  (let ((position (selection-position selection))
        (width (selection-width selection)))
    (flet ((%intersecting-p (center)
             (intersecting-circle-p center radius cursor-position)))
      (cond
        ((%intersecting-p (add position (vec2 width 0))) :bottom-right)
        (t nil)))))
