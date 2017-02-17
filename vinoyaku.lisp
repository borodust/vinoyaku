(in-package :vinoyaku)


(declaim (special *context*))


(defclass vinoyaku-context ()
  ((translator :initform (make-instance 'transltr-client) :reader translator-of)))


(defun make-context ()
  (make-instance 'vinoyaku-context))


(defun preprocess-raw-text (text)
  (loop for i from 0
     for ch across text
     ;; tesseract just can't stop missing this one with default .traineddata
     when (char= ch #\〈)
     do (setf (aref text i) #\く))
  text)


(defun explain (context x y width height)
  (let* ((raw (preprocess-raw-text (tesserect:recognize x y width height)))
         (translated (translate (translator-of context) raw))
         (syllabograms (syllabograms raw)))
    (format t "~%~A~%~A~%~A" raw syllabograms translated))
  (values))
