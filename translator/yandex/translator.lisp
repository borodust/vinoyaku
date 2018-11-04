(cl:defpackage :vinoyaku.translator.yandex
  (:use :cl :vinoyaku.api))
(cl:in-package :vinoyaku.translator.yandex)


(alexandria:define-constant +user-agent+ "vinoyaku-translator-module"
  :test #'string=)


(defvar *api-address* "https://translate.yandex.net/api/v1.5/tr.json/translate")

(defvar *api-key*
  "trnsl.1.1.20181103T125647Z.6e840d50ac9abee5.48c37d8dbf25a04d9e1f585eba1ea7c8efd76534")


(register-translator 'yandex-translate-client)

(defclass yandex-translate-client () ())


(defun send-request (text)
  (let* ((content (format nil "text=~A" (drakma:url-encode text :utf-8)))
         (result (drakma:http-request (format nil "~A?key=~A&lang=ja-en" *api-address* *api-key*)
                                      :method :post
                                      :content content
                                      :content-type "application/x-www-form-urlencoded"
                                      :user-agent +user-agent+
                                      :external-format-out :utf-8
                                      :force-binary t)))
    (babel:octets-to-string result :encoding :utf-8)))


(defmethod translate ((this yandex-translate-client) (text string))
  (let ((result (send-request text)))
    (first (jsown:val (jsown:parse result) "text"))))
