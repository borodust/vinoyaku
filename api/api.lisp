(cl:defpackage :vinoyaku.api
  (:use :cl)
  (:export #:recognize
           #:init-recognizer
           #:discard-recognizer
           #:register-recognizer
           #:list-recognizers

           #:analyze
           #:part-of-speech
           #:pronunciation
           #:original-form
           #:source-form

           #:init-analyzer
           #:discard-analyzer

           #:register-analyzer
           #:list-analyzers

           #:translate
           #:init-translator
           #:discard-translator

           #:register-translator
           #:list-translators))
(cl:in-package :vinoyaku.api)

(defvar *recognizers* nil)

(defun register-recognizer (class-name)
  (pushnew class-name *recognizers*))

(defun list-recognizers ()
  *recognizers*)

(defgeneric init-recognizer (instance)
  (:method (instance) (declare (ignore instance))))
(defgeneric discard-recognizer (instance)
  (:method (instance) (declare (ignore instance))))
(defgeneric recognize (recognizer image width height &key &allow-other-keys))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *analyzers* nil)

(defun register-analyzer (class-name)
  (pushnew class-name *analyzers*))

(defun list-analyzers ()
  *analyzers*)

(defgeneric init-analyzer (instance)
  (:method (instance) (declare (ignore instance))))
(defgeneric discard-analyzer (instance)
  (:method (instance) (declare (ignore instance))))
(defgeneric analyze (analyzer text))

(defgeneric source-form (morph))
(defgeneric part-of-speech (morph))
(defgeneric original-form (morph))
(defgeneric pronunciation (morph))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *translators* nil)

(defun register-translator (class-name)
  (pushnew class-name *translators*))

(defun list-translators ()
  *translators*)

(defgeneric init-translator (instance)
  (:method (instance) (declare (ignore instance))))
(defgeneric discard-translator (instance)
  (:method (instance) (declare (ignore instance))))
(defgeneric translate (translator text))
