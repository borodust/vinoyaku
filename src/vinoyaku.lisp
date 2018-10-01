(cl:in-package :vinoyaku)


(declaim (special *context*))


(defclass vinoyaku-context ()
  ((translator :initform (make-instance 'transltr-client) :reader translator-of)))


(defun make-context ()
  (make-instance 'vinoyaku-context))


(defun preprocess-raw-text (text)
  (flet ((whitespacep (c) (find c '(#\Space #\Newline #\Tab) :test #'char=)))
    (loop with text = (delete-if #'whitespacep text)
       for i from 0
       for ch across text
       ;; tesseract just can't stop missing this one with default .traineddata
       when (char= ch #\〈) do (setf (aref text i) #\く)
       finally (return text))))


(defun explain (context image)
  (let* ((raw (preprocess-raw-text ""#++(recognizr:recognize image)))
         (syllabograms (syllabograms raw))
         (translated ""))
    (handler-case
        (setf translated (translate (translator-of context) raw))
      (error (e)
        (log:error "Couldn't translate text: " e)))
    (format nil "~A~%~A~%~A" raw syllabograms translated)))
