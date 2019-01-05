(cl:in-package :vinoyaku.app.histogram)


(alexandria:define-constant +selection-border-size+ 4)
(alexandria:define-constant +threshold-border-value+ 0.025)


(defclass histogram-selection ()
  ((background-color :initform (vec4 0.8 0.8 0 0.08))
   (border-color :initform (vec4 0.8 0.8 0 0.7))
   (start :initform 10 :reader %start-of)
   (end :initform 100 :reader %end-of)
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
  (with-slots (end start) selection
    (let ((target (+ start offset)))
      (when (< target end)
        (setf start target)))))


(defun histogram-selection-adjust-end (selection offset)
  (with-slots (end start) selection
    (let ((target (+ end offset)))
      (when (> target start)
        (setf end target)))))


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

(defclass histogram-threshold ()
  ((color :initform (vec4 0.9 0.3 0.3 0.7))
   (value :initform 0.01 :reader %value-of)
   (highlighted :initform nil :accessor histogram-threshold-highlighted-p)))


(defun render-histogram-threshold (threshold offset width height)
  (with-slots (value color highlighted) threshold
    (let ((threshold-height (* height value))
          (final-color (if highlighted
                           (add color (vec4 0.1 -0.3 -0.3 0.3))
                           color)))
      (bodge-canvas:draw-line (add offset (vec2 0 threshold-height))
                              (add offset (vec2 width threshold-height))
                              final-color
                              :thickness 1.5))))


(defun histogram-threshold-intersects (threshold value)
  (with-slots ((current-value value)) threshold
    (let ((half (/ +threshold-border-value+ 2)))
      (<= (- current-value half) value (+ current-value half)))))


(defun histogram-threshold-adjust (threshold offset)
  (with-slots (value) threshold
    (incf value offset)
    (alexandria:maxf value 0)))

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
   (selection :initform (make-instance 'histogram-selection) :reader %selection-of)
   (threshold :initform (make-instance 'histogram-threshold) :reader %threshold-of)
   (last-width :initform 0)))


(defmethod initialize-instance :after ((this histogram) &key)
  (bodge-ui:transition-custom-widget-to this 'rest-state))


(defmethod bodge-ui:custom-widget-height ((this histogram))
  128)


(defmethod bodge-ui:render-custom-widget ((this histogram) origin width height)
  (with-slots (array selection state threshold last-width) this
    (render-histogram-array array origin width height)
    (render-histogram-selection selection origin height)
    (render-histogram-threshold threshold origin width height)
    (setf last-width width)))


(defun update-histogram-array (histogram array)
  (with-slots ((histogram-array array)) histogram
    (setf (array-of histogram-array) array)))


(defun histogram-threshold (histogram)
  (with-slots (threshold) histogram
    (%value-of threshold)))


(defun histogram-bounds (histogram)
  (with-slots (array selection last-width) histogram
    (let ((length (length (array-of array)))
          (width (max last-width 1)))
      (flet ((%scale (value)
               (floor (* (/ value width) length))))
        (values (%scale (%start-of selection)) (%scale (%end-of selection)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass rest-state ()
  ((selection-highlighted-p :initform nil)
   (threshold-highlighted-p :initform nil)
   (offset-x :initform 0)
   (offset-y :initform 0)))


(defmethod bodge-ui:custom-widget-on-mouse-press ((this rest-state) button)
  (with-slots (selection-highlighted-p threshold-highlighted-p offset-x offset-y) this
    (let ((widget (bodge-ui:custom-widget-instance)))
      (when selection-highlighted-p
        (let ((selection (%selection-of widget)))
          (cond
            ((histogram-selection-intersects-left-border selection offset-x)
             (bodge-ui:transition-custom-widget-to widget 'adjust-selection-start-state :offset offset-x))
            ((histogram-selection-intersects-right-border selection offset-x)
             (bodge-ui:transition-custom-widget-to widget 'adjust-selection-end-state :offset offset-x))
            (t (bodge-ui:transition-custom-widget-to widget 'move-selection-state :offset offset-x)))))
      (when threshold-highlighted-p
        (bodge-ui:transition-custom-widget-to widget 'adjust-threshold-state :offset (/ offset-y (bodge-ui:custom-widget-height widget)))))))


(defmethod bodge-ui:custom-widget-on-move ((this rest-state) x y)
  (with-slots (selection-highlighted-p threshold-highlighted-p offset-x offset-y) this
    (let* ((widget (bodge-ui:custom-widget-instance))
           (selection (%selection-of widget))
           (threshold (%threshold-of widget)))
      (setf selection-highlighted-p (histogram-selection-intersects-body selection x)
            (histogram-selection-highlighted-p selection) selection-highlighted-p
            threshold-highlighted-p (histogram-threshold-intersects threshold (/ y (bodge-ui:custom-widget-height widget)))
            (histogram-threshold-highlighted-p threshold) threshold-highlighted-p
            offset-x x
            offset-y y))))


(defmethod bodge-ui:custom-widget-on-leave ((this rest-state))
  (with-slots (selection-highlighted-p threshold-highlighted-p) this
    (let* ((widget (bodge-ui:custom-widget-instance))
           (selection (%selection-of widget))
           (threshold (%threshold-of widget)))
      (setf selection-highlighted-p nil
            (histogram-selection-highlighted-p selection) nil
            threshold-highlighted-p nil
            (histogram-threshold-highlighted-p threshold) nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass move-selection-state ()
  ((current-offset :initarg :offset :initform 0)))


(defmethod bodge-ui:custom-widget-on-move ((this move-selection-state) x y)
  (declare (ignore y))
  (with-slots (current-offset) this
    (let ((selection (%selection-of (bodge-ui:custom-widget-instance))))
      (histogram-selection-move selection (- x current-offset))
      (setf current-offset x))))


(defmethod bodge-ui:custom-widget-on-mouse-release ((this move-selection-state) button)
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))


(defmethod bodge-ui:custom-widget-on-leave ((this move-selection-state))
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass adjust-selection-start-state ()
  ((offset :initarg :offset :initform 0)))


(defmethod bodge-ui:custom-widget-on-move ((this adjust-selection-start-state) x y)
  (declare (ignore y))
  (with-slots (offset) this
    (let ((selection (%selection-of (bodge-ui:custom-widget-instance))))
      (histogram-selection-adjust-start selection (- x offset))
      (setf offset x))))


(defmethod bodge-ui:custom-widget-on-leave ((this adjust-selection-start-state))
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))


(defmethod bodge-ui:custom-widget-on-mouse-release ((this adjust-selection-start-state) button)
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass adjust-selection-end-state ()
  ((offset :initarg :offset :initform 0)))


(defmethod bodge-ui:custom-widget-on-move ((this adjust-selection-end-state) x y)
  (declare (ignore y))
  (with-slots (offset) this
    (let ((selection (%selection-of (bodge-ui:custom-widget-instance))))
      (histogram-selection-adjust-end selection (- x offset))
      (setf offset x))))


(defmethod bodge-ui:custom-widget-on-mouse-release ((this adjust-selection-end-state) button)
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))


(defmethod bodge-ui:custom-widget-on-leave ((this adjust-selection-end-state))
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass adjust-threshold-state ()
  ((offset :initarg :offset :initform 0)))


(defmethod bodge-ui:custom-widget-on-move ((this adjust-threshold-state) x y)
  (declare (ignore x))
  (with-slots (offset) this
    (let* ((widget (bodge-ui:custom-widget-instance))
           (threshold (%threshold-of widget))
           (new-offset (/ y (bodge-ui:custom-widget-height widget))))
      (histogram-threshold-adjust threshold (- new-offset offset))
      (setf offset new-offset))))


(defmethod bodge-ui:custom-widget-on-mouse-release ((this adjust-threshold-state) button)
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))


(defmethod bodge-ui:custom-widget-on-leave ((this adjust-threshold-state))
  (bodge-ui:transition-custom-widget-to (bodge-ui:custom-widget-instance) 'rest-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
