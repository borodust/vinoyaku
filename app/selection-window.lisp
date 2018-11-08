(cl:in-package :vinoyaku.app)


(defclass selection-window (ui-window)
  (state
   (cursor-type :initform nil)
   (context :initarg :context :reader context-of))
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
  (with-slots (state) this
    (setf state (make-instance 'rest-state :selection (make-selection)))))


(defmethod bodge-host:on-init ((this selection-window))
  (let* ((monitor (bodge-host:window-monitor this))
         (video-mode (bodge-host:monitor-video-mode monitor)))
    (setf (bodge-host:viewport-position this) (bodge-host:monitor-position monitor)
          (bodge-host:viewport-size this) (vec2 (bodge-host:video-mode-width video-mode)
                                                (bodge-host:video-mode-height video-mode)))))


(defmethod bodge-host:on-cursor-movement ((this selection-window) x y)
  (with-slots (state) this
    (let ((*window* this))
      (when state
        (on-cursor-movement state x y)))))


(defmethod bodge-host:on-mouse-action ((this selection-window) button button-state)
  (with-slots (state) this
    (let ((*window* this))
      (when state
       (on-mouse-action state button button-state)))))


(defmethod on-draw ((this selection-window))
  (with-slots (state) this
    (let ((*window* this))
      (gl:viewport 0 0 (canvas-width) (canvas-height))
      (gl:clear-color 0.0 0.0 0.0 0.0)
      (gl:clear :color-buffer)
      (render-state state))))


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
