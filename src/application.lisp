(cl:in-package :vinoyaku)

(declaim (special *window*))

(defvar *window-width* 800)
(defvar *window-height* 600)

(defclass main-window (ui-window) ()
  (:default-initargs
   :title "びの訳"
   :width *window-width*
   :height *window-height*))


(defmethod on-rendering-context-ready ((this main-window))
  (add-window-panel this 'control-panel))


(defmethod on-draw ((this main-window))
  (gl:clear-color 0.2 0.2 0.2 1)
  (gl:clear :color-buffer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric render-state (state)
  (:method (state) (declare (ignore state))))
(defgeneric on-cursor-movement (state x y)
  (:method (state x y) (declare (ignore state x y))))
(defgeneric on-mouse-action (selection-state button state)
  (:method (selection-state button state) (declare (ignore selection-state button state))))
(defgeneric on-key-action (selection-state key state)
  (:method (selection-state button state) (declare (ignore selection-state button state))))



(defclass selection-window (ui-window)
  (canvas
   state
   (cursor-type :initform nil))
  (:default-initargs
   :title "Selection Window"))


(defun set-cursor (cursor)
  (let ((window *window*))
    (with-slots (cursor-type) window
      (unless (eq cursor-type cursor)
        (bodge-host:progm
          (alexandria:when-let ((current-cursor (bodge-host:cursor window)))
            (bodge-host:destroy-cursor current-cursor))
          (setf (bodge-host:cursor window) (bodge-host:make-standard-cursor cursor)
                cursor-type cursor))))))


(defmethod on-rendering-context-ready ((this selection-window))
  (with-slots (canvas state) this
    (setf canvas (bodge-canvas:make-canvas 640 480)
          state (make-instance 'rest-state :selection (make-selection)))))


(defmethod bodge-host:on-init ((this selection-window))
  (let* ((monitor (bodge-host:window-monitor this))
         (video-mode (bodge-host:monitor-video-mode monitor)))
    (setf (bodge-host:viewport-position this) (bodge-host:monitor-position monitor)
          (bodge-host:viewport-size this) (vec2 (bodge-host:video-mode-width video-mode)
                                                (bodge-host:video-mode-height video-mode)))))


(defmethod bodge-host:on-viewport-size-change ((this selection-window) width height)
  (with-slots (canvas) this
    (within-rendering-thread (this)
      (bodge-canvas:update-canvas-size canvas width height))))


(defmethod bodge-host:on-cursor-movement ((this selection-window) x y)
  (with-slots (state) this
    (let ((*window* this))
      (on-cursor-movement state x y))))


(defmethod bodge-host:on-mouse-action ((this selection-window) button button-state)
  (with-slots (state) this
    (let ((*window* this))
      (on-mouse-action state button button-state))))


(defmethod on-draw ((this selection-window))
  (with-slots (canvas state) this
    (let ((*window* this))
      (gl:viewport 0 0 (canvas-width) (canvas-height))
      (gl:clear-color 0.0 0.0 0.0 0.0)
      (gl:clear :color-buffer)
      (bodge-canvas:with-canvas (canvas)
        (render-state state)))))


(defmethod bodge-host:on-key-action ((this selection-window)
                                     (key (eql :escape))
                                     (state (eql :released)))
  (declare (ignore key state))
  (bodge-host:hide-window this))


(defmethod bodge-host:on-key-action ((this selection-window) key key-state)
  (with-slots (state) this
    (let ((*window* this))
      (on-key-action state key key-state ))))


(defun transition-to (state-class &rest initargs &key &allow-other-keys)
  (with-slots (state) *window*
    (bodge-host:progm
      (setf state (apply #'make-instance state-class initargs)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass base-state ()
  ((selection :initarg :selection)))


(defmethod render-state ((this base-state))
  (with-slots (selection) this
    (draw-selection-and-cursor selection)))


(defclass rest-state (base-state) ())


(defun selection-hovered-p (selection)
  (let ((cursor-position (bodge-host:cursor-position *window*)))
    (intersecting-selection-p selection cursor-position)))


(defun draw-selection-and-cursor (selection)
  (if (selection-hovered-p selection)
      (progn
        (set-cursor :hand)
        (setf (selection-transform-mode-p selection) t)
        (draw-selection selection))
      (progn
        (set-cursor :arrow)
        (setf (selection-transform-mode-p selection) nil)
        (draw-selection selection))))


(defmethod on-mouse-action ((this rest-state) (button (eql :left)) (state (eql :pressed)))
  (with-slots (selection) this
    (let* ((cursor-position (bodge-host:cursor-position *window*))
           (corner (intersecting-corner-p selection cursor-position)))
      (if corner
          (transition-to 'resize-state :selection selection
                                       :cursor-position cursor-position
                                       :corner corner)
          (when (intersecting-selection-p selection cursor-position)
            (transition-to 'move-state :selection selection
                                       :cursor-position cursor-position))))))


(defmethod on-key-action ((this rest-state)
                          (key (eql :enter))
                          (state (eql :pressed)))
  (declare (ignore key state))
  (with-slots (selection) this
    (let ((pos (bodge-host:viewport-position *window*)))
      (within-rendering-thread (*window*)
        (read-selected-region-into-rgba-image selection (y pos))))))


(defclass move-state (base-state)
  ((initial-offset)))


(defmethod initialize-instance :after ((this move-state) &key cursor-position)
  (with-slots (initial-offset selection) this
    (setf initial-offset (subt cursor-position (selection-position selection)))))


(defmethod on-cursor-movement ((this move-state) x y)
  (with-slots (selection initial-offset) this
    (setf (x (selection-position selection)) (- x (x initial-offset))
          (y (selection-position selection)) (- y (y initial-offset)))))


(defmethod on-mouse-action ((this move-state) (button (eql :left)) (state (eql :released)))
  (with-slots (selection) this
    (transition-to 'rest-state :selection selection)))


(defclass resize-state (base-state)
  ((corner :initarg :corner)
   (prev-cursor-position)))


(defmethod initialize-instance :after ((this resize-state) &key cursor-position)
  (with-slots (prev-cursor-position selection) this
    (setf prev-cursor-position cursor-position)))


(defmethod on-cursor-movement ((this resize-state) x y)
  (with-slots (selection prev-cursor-position corner) this
    (let ((width-offset (- x (x prev-cursor-position)))
          (height-offset (- y (y prev-cursor-position))))
      (when (eq corner :bottom-right)
        (when (or (> (selection-width selection) 10)
                  (> width-offset 0))
          (incf (selection-width selection) width-offset))
        (when (or (> (selection-height selection) 10)
                  (< height-offset 0))
          (incf (selection-height selection) (- height-offset))
          (incf (y (selection-position selection)) height-offset)))
      (setf (x prev-cursor-position) x
            (y prev-cursor-position) y))))


(defmethod on-mouse-action ((this resize-state) (button (eql :left)) (state (eql :released)))
  (with-slots (selection) this
    (transition-to 'rest-state :selection selection)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun run ()
  (bodge-host:open-window (make-instance 'main-window)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun open-selection-window (win)
  (declare (ignore win))
  (bodge-host:open-window (make-instance 'selection-window
                                         :transparent t
                                         :decorated nil
                                         :resizable t
                                         :floating t)))


(bodge-ui:defwindow (control-panel
                     (:title "YO")
                     (:origin 50 100)
                     (:width 400) (:height 72)
                     (:options :movable))
  (bodge-ui:button :label "HI" :on-click #'open-selection-window))
