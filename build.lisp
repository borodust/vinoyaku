(cl:in-package :cl-user)


(ql:quickload :vinoyaku)


(defun target-file ()
  (loop for arg on *command-line-argument-list*
     until (equal (first arg) "--")
     finally (return (cadr arg))))


(defun build ()
  (ccl:save-application (target-file) :toplevel-function #'vinoyaku::main :prepend-kernel t))


(build)
