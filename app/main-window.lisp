(cl:in-package :vinoyaku.app)

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
