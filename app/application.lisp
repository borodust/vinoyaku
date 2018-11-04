(cl:in-package :vinoyaku.app)



(defun run ()
  (bodge-host:open-window (make-instance 'main-window)))
