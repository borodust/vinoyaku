(in-package :vinoyaku)


(defgeneric translate (client text))


(define-constant +transltr-api-address+ "http://www.transltr.org/api/"
  :test #'string=)


(define-constant +user-agent+ "grateful-transltr-client"
  :test #'string=)


(defclass transltr-client ()
  ((translate-addr :initform (format nil "~A~A"
                                       +transltr-api-address+
                                       "translate"))))


(defmethod translate ((this transltr-client) (text string))
  (with-slots (translate-addr headers) this
    (handler-case
        (let* ((content (jsown:to-json (jsown:new-js ("text" text)
                                                     ("to" "en")
                                                     ("from" "ja"))))
               (answer (drakma:http-request translate-addr
                                            :method :post
                                            :content content
                                            :content-type "application/json"
                                            :user-agent +user-agent+
                                            :external-format-out :utf-8
                                            :force-binary t))
               (json (octets-to-string answer :external-format :utf-8)))
          (jsown:val (jsown:parse json) "translationText"))
      (t (e) (log:error "Error during translation request: ~A" e) nil))))
