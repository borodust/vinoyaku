(cl:in-package :vinoyaku.app)


(declaim (special *window*))


(defgeneric render-state (state)
  (:method (state) (declare (ignore state))))

(defgeneric on-cursor-movement (state x y)
  (:method (state x y) (declare (ignore state x y))))

(defgeneric on-mouse-action (selection-state button state)
  (:method (selection-state button state) (declare (ignore selection-state button state))))

(defgeneric on-key-action (selection-state key state)
  (:method (selection-state button state) (declare (ignore selection-state button state))))


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
    (let ((pos (bodge-host:viewport-position *window*))
          (win *window*))
      (within-rendering-thread (win)
        (let ((*window* win))
          (read-selected-region-into-rgba-image selection (y pos)))))))


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
