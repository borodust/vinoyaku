(cl:in-package :vinoyaku)


(defgeneric on-draw (window)
  (:method (window) (declare (ignore window))))


(defgeneric on-rendering-context-ready (window)
  (:method (window) (declare (ignore window))))


(defclass ui-window (bodge-host:window)
  ((context :initform nil)
   (renderer :initform nil)
   (enabled-p :initform t)
   (mouse-actions :initform (list))
   (cursor-position :initform (bodge-math:vec2))
   (context-queue :initform (bodge-concurrency:make-task-queue)))
  (:default-initargs :opengl-version '(2 1)))


(defun push-context-task (window task)
  (with-slots (context-queue) window
    (bodge-concurrency:push-task task context-queue)))


(defmacro within-rendering-thread ((window) &body body)
  `(push-context-task ,window (lambda () ,@body)))


(defun add-window-panel (window panel-class &rest initargs &key &allow-other-keys)
  (with-slots (context) window
    (apply #'bodge-ui:add-window context panel-class initargs)))


(defun setup-rendering-context (window)
  (bodge-host:bind-main-rendering-context window)
  (setf (bodge-host:swap-interval) 1)
  (glad:init))


(defun initialize-ui (window)
  (with-slots (context renderer) window
    (setf renderer (bodge-canvas-ui:make-renderer *window-width* *window-height*)
          context (bodge-ui:make-ui renderer :input-source window))))


(defun release-ui (window)
  (with-slots (context renderer) window
    (bodge-memory:dispose context)
    (bodge-canvas-ui:destroy-renderer renderer)))


(defun render-ui (window)
  (with-slots (context) window
    (on-draw window)
    (bodge-ui:compose-ui context)
    (bodge-host:swap-buffers window)))


(defun run-rendering-loop (window)
  (with-slots (enabled-p context-queue) window
    (tagbody begin
       (restart-case
           (loop while enabled-p
                 do (bodge-concurrency:drain context-queue)
                    (render-ui window))
         (continue-rendering ()
           :report "Restart rendering loop"
           (setf enabled-p t)
           (go begin))
         (stop-rendering ()
           :report "Stop rendering loop"
           (setf enabled-p nil)
           (go end)))
     end)))


(defun start-rendering-thread (window)
  (with-slots (context renderer enabled-p) window
    (bodge-concurrency:in-new-thread ("rendering-thread")
      (unwind-protect
           (progn
             (setup-rendering-context window)
             (initialize-ui window)
             (on-rendering-context-ready window)
             (unwind-protect
                  (run-rendering-loop window)
               (release-ui window)))
        (bodge-host:close-window window)))))


(defmethod bodge-host:on-init :around ((this ui-window))
  (with-slots (context renderer enabled-p) this
    (setf enabled-p t)
    (start-rendering-thread this))
  (call-next-method))


(defmethod bodge-host:on-hide :around ((this ui-window))
  (with-slots (enabled-p) this
    (setf enabled-p nil))
  (call-next-method))


(defmethod bodge-host:on-destroy :around ((this ui-window))
  (with-slots (enabled-p) this
    (setf enabled-p nil))
  (call-next-method))


(defmethod bodge-host:on-mouse-action :around ((this ui-window) button action)
  (with-slots (mouse-actions) this
    (alexandria:nconcf mouse-actions (list (cons button action))))
  (call-next-method))


(defmethod bodge-host:on-cursor-movement :around ((this ui-window) x y)
  (with-slots (cursor-position) this
    (setf (bodge-math:x cursor-position) x
          (bodge-math:y cursor-position) y))
  (call-next-method))


(defmethod bodge-ui:next-mouse-interaction ((this ui-window))
  (with-slots (mouse-actions) this
    (let ((interaction (pop mouse-actions)))
      (values (car interaction) (cdr interaction)))))


(defmethod bodge-ui:last-cursor-position ((this ui-window)
                                          &optional (result-vec2 (bodge-math:vec2)))
  (with-slots (cursor-position) this
    (setf (bodge-math:x result-vec2) (bodge-math:x cursor-position)
          (bodge-math:y result-vec2) (bodge-math:y cursor-position))
    result-vec2))
