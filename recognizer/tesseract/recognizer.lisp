(cl:defpackage :vinoyaku.recognizer.tesseract
  (:use :cl :vinoyaku.api))
(cl:in-package :vinoyaku.recognizer.tesseract)


(bodge-util:define-constant +tesseract-directory+
    (namestring (asdf:system-relative-pathname :vinoyaku/recognizer/tesseract
                                               "recognizer/tesseract/"))
  :test #'equal)


(register-recognizer 'tesseract-recognizer)
(defclass tesseract-recognizer ()
  ((context :initform nil)))


(defmethod init-recognizer :after ((this tesseract-recognizer))
  (with-slots (context) this
    ;; setlocale in baseapi.c https://github.com/tesseract-ocr/tesseract/issues/1670
    (bodge-util:with-locale ("C")
      (setf context (%tess:base-api-create))
      (cffi:with-foreign-string (tess-conf-path (namestring
                                                 (merge-pathnames "tess.conf" +tesseract-directory+)))
        (claw:c-with ((tess-configs :pointer :count 1))
          (setf (tess-configs 0) tess-conf-path)
          (assert (= 0 (%tess:base-api-init1 context
                                             +tesseract-directory+ "jpn" %tess:+oem-tesseract-only+
                                             (tess-configs &) 1)))
          (%tess:base-api-set-page-seg-mode context %tess:+psm-single-block+))))))


(defmethod discard-recognizer :before ((this tesseract-recognizer))
  (with-slots (context) this
    (%tess:base-api-end context)
    (%tess:base-api-delete context)
    (setf context nil)))


(defun get-tess-string-variable (context name)
  (cffi:with-foreign-string (foreign-name name)
    (cffi:foreign-string-to-lisp (%tess:base-api-get-string-variable context foreign-name))))


(defun get-tess-int-variable (context name)
  (cffi:with-foreign-string (foreign-name name)
    (claw:c-with ((value :int))
      (%tess:base-api-get-int-variable context foreign-name (value &))
      value)))


(defmethod recognize ((this tesseract-recognizer) image width height &key (format :rgba))
  (with-slots (context) this
    ;; setlocale in baseapi.c https://github.com/tesseract-ocr/tesseract/issues/1670
    (%tess:base-api-get-available-languages-as-vector context)
    (log:info "~A" (get-tess-int-variable context "edges_max_children_per_outline"))
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
