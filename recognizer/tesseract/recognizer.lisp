(cl:defpackage :vinoyaku.recognizer.tesseract
  (:use :cl :vinoyaku.api))
(cl:in-package :vinoyaku.recognizer.tesseract)


(bodge-util:define-constant +tesseract-prefix+
    (namestring (asdf:system-relative-pathname :vinoyaku/recognizer/tesseract
                                               "recognizer/tesseract/"))
  :test #'equal)


(register-recognizer 'tesseract-recognizer)
(defclass tesseract-recognizer ()
  ((context :initform nil)))


(defmacro with-tessdata-prefix ((prefix) &body body)
  (bodge-util:with-gensyms (envar)
    `(let ((,envar (or (uiop:getenv "TESSDATA_PREFIX") "./")))
       (unwind-protect
            (progn
              (setf (uiop:getenv "TESSDATA_PREFIX") ,prefix)
              ,@body)
         (setf (uiop:getenv "TESSDATA_PREFIX") ,envar)))))


(defmethod init-recognizer :after ((this tesseract-recognizer))
  (with-slots (context) this
    ;; setlocale in baseapi.c https://github.com/tesseract-ocr/tesseract/issues/1670
    (with-tessdata-prefix (+tesseract-prefix+)
      (bodge-util:with-locale ("C")
        (setf context (%tess:base-api-create))
        (assert (= 0 (%tess:base-api-init3 context nil "jpn")))
        #++(progn
             (%tess:base-api-set-variable context "chop_enable" "T")
             (%tess:base-api-set-variable context "use_new_state_cost" "F")
             (%tess:base-api-set-variable context "enable_new_segsearch" "0")
             #++(%tess:base-api-set-variable context "language_model_ngram_on" "0")
             #++(%tess:base-api-set-variable context "textord_force_make_prop_words" "F")
             #++(%tess:base-api-set-variable context "edges_max_children_per_outline" "40"))))))



(defmethod discard-recognizer :before ((this tesseract-recognizer))
  (with-slots (context) this
    (%tess:base-api-end context)
    (%tess:base-api-delete context)
    (setf context nil)))


(defmethod recognize ((this tesseract-recognizer) image width height &key (format :rgba))
  (with-slots (context) this
    ;; setlocale in baseapi.c https://github.com/tesseract-ocr/tesseract/issues/1670
    (claw:with-float-traps-masked ()
      (bodge-util:with-locale ("C")
        (let ((bytes-per-pixel (ecase format
                                 (:rgba 4)
                                 (:rgb 3)
                                 ((:grey :red) 1))))
          (bodge-util:with-simple-array-pointer (ptr image)
            (let* ((foreign-result (%tess:base-api-rect context ptr
                                                        bytes-per-pixel (* width bytes-per-pixel)
                                                        0 0 width height))
                   (result-string (cffi:foreign-string-to-lisp foreign-result :encoding :utf-8)))
              ;; as per docs, we should manually free a resulting string
              (claw:free foreign-result)
              (string-trim '(#\Newline #\Space #\Tab) result-string))))))))
