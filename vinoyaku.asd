(cl:pushnew :bodge-gl2 *features*)

(asdf:defsystem vinoyaku
  :description "Tool to assist in VN reading"
  :version "1.0.0"
  :author "Pavel Korolev"
  :license "MIT"
  :depends-on (alexandria mecab-blob bodge-mecab tesseract-blob bodge-tesseract bodge-glad glad-blob
                          bodge-utilities bodge-host bodge-ui bodge-canvas bodge-canvas-ui
                          bodge-memory bodge-concurrency
                          drakma jsown claw flexi-streams log4cl trivial-main-thread dissect opticl)
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "translation")
               (:file "recognition")
               (:file "morph")
               (:file "vinoyaku")
               (:file "ui-window")
               (:file "application")))
