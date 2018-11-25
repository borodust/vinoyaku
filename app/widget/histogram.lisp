(cl:in-package :vinoyaku.app.histogram)


(alexandria:define-constant +selection-border-size+ 4)


(defclass histogram-selection ()
  ((background-color :initform (vec4 0.8 0.8 0 0.08))
   (border-color :initform (vec4 0.8 0.8 0 0.7))
   (start :initform 10)
   (end :initform 100)
   (highlighted :initform nil :accessor histogram-selection-highlighted-p)))


(defun histogram-selection-intersects-body (selection x)
  (with-slots (start end) selection
    (<= start x end)))


(defun histogram-selection-intersects-left-border (selection x)
  (with-slots (start) selection
    (let ((half (/ +selection-border-size+ 2)))
      (<= (- start half) x (+ start half)))))


(defun histogram-selection-intersects-right-border (selection x)
  (with-slots (end) selection
    (let ((half (/ +selection-border-size+ 2)))
      (<= (- end half) x (+ end half)))))


(defun histogram-selection-adjust-start (selection offset)
  (with-slots (start) selection
    (incf start offset)))


(defun histogram-selection-adjust-end (selection offset)
  (with-slots (end) selection
    (incf end offset)))


(defun histogram-selection-move (selection offset)
  (histogram-selection-adjust-start selection offset)
  (histogram-selection-adjust-end selection offset))



(defun render-histogram-selection (selection offset height)
  (with-slots (background-color border-color start end highlighted) selection
    (let ((region-origin (add offset (vec2 start)))
          (region-end (add offset (vec2 end)))
          (background-color (if highlighted
                                (add background-color (vec4 0.5 0.5))
                                background-color))
          (border-color (if highlighted
                            (add border-color (vec4 0.5 0.5))
                            border-color)))
      (bodge-canvas:draw-rect region-origin
                              (abs (- end start))
                              height
                              :fill-paint background-color)
      (bodge-canvas:draw-line region-origin
                              (add region-origin (vec2 0 height))
                              border-color)
      (bodge-canvas:draw-line region-end
                              (add region-end (vec2 0 height))
                              border-color))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass histogram-array ()
  ((array :initform #() :accessor array-of)
   (background :initform (vec4 0.15 0.15 0.15 1))))


(defmethod initialize-instance :after ((this histogram-array) &key)
  (with-slots (array) this
    (let ((a (make-array 256)))
      (loop for i below (length a)
            do (setf (aref a i) i))
      (setf array a))))


(defun render-histogram-array (this origin width height)
  (with-slots (array background) this
    (bodge-canvas:draw-rect origin width height
                            :fill-paint background
                            :rounding 4)
    (let ((col-width (/ width (if (alexandria:emptyp array)
                                  1
                                  (length array))))
          (max-height (reduce #'max array :initial-value 0))
          (half-margin 0.5))
      (loop for value across array
            for i from 0
            for x-offset = (* i col-width)
            when (> value 0)
              do (bodge-canvas:draw-rect (add origin (vec2 (+ x-offset half-margin) 0))
                                         (- col-width half-margin)
                                         (* (/ value max-height) height)
                                         :fill-paint (vec4 0.85 0.85 0.85 1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass histogram (bodge-ui:custom-widget)
  ((array :initform (make-instance 'histogram-array) :reader %array-of)
   (selection :initform (make-instance 'histogram-selection) :reader %selection-of)))


(defmethod initialize-instance :after ((this histogram) &key)
  (bodge-ui:transition-custom-widget-to this 'rest-state))


(defmethod bodge-ui:custom-widget-height ((this histogram))
  128)


(defmethod bodge-ui:render-custom-widget ((this histogram) origin width height)
  (with-slots (array selection state) this
    (render-histogram-array array origin width height)
    (render-histogram-selection selection origin height)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rest-state ()
  ((selection-highlighted-p :initform nil)
   (offset :initform 0)))


(defmethod bodge-ui:custom-widget-on-mouse-press ((this rest-state) button)
  (with-slots (selection-highlighted-p offset) this
    (when selection-highlighted-p
      (let ((selection (%selection-of (bodge-ui:custom-widget-instance))))
        (cond
          ((histogram-selection-intersects-left-border selection offset)
           (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'adjust-start-state :offset offset))
          ((histogram-selection-intersects-right-border selection offset)
           (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'adjust-end-state :offset offset))
          (t (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'move-state :offset offset)))))))


(defmethod bodge-ui:custom-widget-on-move ((this rest-state) x y)
  (declare (ignore y))
  (with-slots (selection-highlighted-p offset) this
    (let ((selection (%selection-of (bodge-ui:custom-widget-instance))))
      (setf selection-highlighted-p (histogram-selection-intersects-body selection x)
            (histogram-selection-highlighted-p selection) selection-highlighted-p
            offset x))))


(defmethod bodge-ui:custom-widget-on-leave ((this rest-state))
  (with-slots (selection-highlighted-p) this
    (let ((selection (%selection-of (bodge-ui:custom-widget-instance))))
      (setf selection-highlighted-p nil
            (histogram-selection-highlighted-p selection) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass move-state ()
  ((current-offset :initarg :offset :initform 0)))


(defmethod bodge-ui:custom-widget-on-move ((this move-state) x y)
  (declare (ignore y))
  (with-slots (current-offset) this
    (let ((selection (%selection-of (bodge-ui:custom-widget-instance))))
      (histogram-selection-move selection (- x current-offset))
      (setf current-offset x))))


(defmethod bodge-ui:custom-widget-on-mouse-release ((this move-state) button)
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))


(defmethod bodge-ui:custom-widget-on-leave ((this move-state))
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass adjust-start-state ()
  ((offset :initarg :offset :initform 0)))


(defmethod bodge-ui:custom-widget-on-move ((this adjust-start-state) x y)
  (declare (ignore y))
  (with-slots (offset) this
    (let ((selection (%selection-of (bodge-ui:custom-widget-instance))))
      (histogram-selection-adjust-start selection (- x offset))
      (setf offset x))))


(defmethod bodge-ui:custom-widget-on-leave ((this adjust-start-state))
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))


(defmethod bodge-ui:custom-widget-on-mouse-release ((this adjust-start-state) button)
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass adjust-end-state ()
  ((offset :initarg :offset :initform 0)))


(defmethod bodge-ui:custom-widget-on-move ((this adjust-end-state) x y)
  (declare (ignore y))
  (with-slots (offset) this
    (let ((selection (%selection-of (bodge-ui:custom-widget-instance))))
      (histogram-selection-adjust-end selection (- x offset))
      (setf offset x))))


(defmethod bodge-ui:custom-widget-on-mouse-release ((this adjust-end-state) button)
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))


(defmethod bodge-ui:custom-widget-on-leave ((this adjust-end-state))
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
