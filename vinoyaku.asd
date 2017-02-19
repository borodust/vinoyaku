(in-package :cl-user)

(defpackage :vinoyaku.def
  (:use :cl :asdf))


(in-package :vinoyaku.def)


(defsystem vinoyaku
  :description "Tool to assist in VN reading"
  :version "0.0.1"
  :author "Pavel Korolev"
  :license "MIT"
  :depends-on (alexandria cl-mecab tesserect drakma jsown cl-autowrap cl-plus-c cffi
                          flexi-streams log4cl trivial-main-thread
                          (:feature (:and :ccl :darwin)
                                    (:require :objc-support)))
  :serial t
  :components ((:file "packages")
               (:file "translation")
               (:file "morph")
               (:file "vinoyaku")
               (:file "ccl-application" :if-feature (:and :ccl :darwin))))
