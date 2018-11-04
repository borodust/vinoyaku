(cl:defpackage :vinoyaku
  (:use :cl :vinoyaku.api)
  (:export #:make-context
           #:destroy-context
           #:explain))
(cl:in-package :vinoyaku)


(declaim (special *context*))


(bodge-util:define-constant +channels+ 4)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass vinoyaku-context ()
  ((recognizer :initarg :recognizer)
   (analyzer :initarg :analyzer)
   (translator :initarg :translator)))


(defmethod initialize-instance :after ((this vinoyaku-context) &key recognizer-class
                                                                 analyzer-class
                                                                 translator-class)
  (with-slots (recognizer analyzer translator) this
    (setf recognizer (make-instance (or recognizer-class
                                        (first (list-recognizers))))
          analyzer (make-instance (or analyzer-class
                                      (first (list-analyzers))))
          translator (make-instance (or translator-class
                                        (first (list-translators)))))
    (init-recognizer recognizer)
    (init-analyzer analyzer)
    (init-translator translator)))


(defun make-context (&key recognizer-class analyzer-class translator-class)
  (make-instance 'vinoyaku-context
                 :recognizer recognizer-class
                 :analyzer analyzer-class
                 :translator translator-class))


(defun destroy-context (context)
  (with-slots (recognizer analyzer translator) context
    (discard-recognizer recognizer)
    (discard-analyzer analyzer)
    (discard-translator translator)))


(defun explain (context image width height &key (format :rgba))
  (with-slots (recognizer analyzer translator) context
    (let* ((raw (recognize recognizer image width height :format format))
           (morphs (analyze analyzer raw))
           (translated (translate translator raw)))
      (values raw morphs translated))))
