(cl:pushnew :bodge-gl2 *features*)


(asdf:defsystem vinoyaku/api
  :description "vinoyaku API"
  :version "1.0.0"
  :author "Pavel Korolev"
  :license "MIT"
  :depends-on ()
  :serial t
  :pathname "api/"
  :components ((:file "api")))


(asdf:defsystem vinoyaku
  :description "Tool to assist in VN reading"
  :version "1.0.0"
  :author "Pavel Korolev"
  :license "MIT"
  :depends-on (vinoyaku/api cffi)
  :serial t
  :pathname "src/"
  :components ((:file "vinoyaku")))


(asdf:defsystem vinoyaku/translator/yandex
  :description "Yandex-powered vinoyaku translator"
  :version "1.0.0"
  :author "Pavel Korolev"
  :license "MIT"
  :depends-on (vinoyaku/api babel drakma jsown)
  :serial t
  :pathname "translator/yandex/"
  :components ((:file "translator")))


(asdf:defsystem vinoyaku/recognizer/tesseract
  :description "Tesseract OCR based vinoyaku recognizer"
  :version "1.0.0"
  :author "Pavel Korolev"
  :license "MIT"
  :depends-on (vinoyaku/api bodge-utilities claw tesseract-blob-v3 bodge-tesseract-v3)
  :serial t
  :pathname "recognizer/tesseract/"
  :components ((:file "recognizer")))


(asdf:defsystem vinoyaku/analyzer/mecab
  :description "MeCab-powered vinoyaku morphological analyzer"
  :version "1.0.0"
  :author "Pavel Korolev"
  :license "MIT"
  :depends-on (vinoyaku/api bodge-utilities claw mecab-blob bodge-mecab)
  :serial t
  :pathname "analyzer/mecab/"
  :components ((:file "analyzer")))


(asdf:defsystem vinoyaku/app
  :description "GUI application for vinoyaku"
  :version "1.0.0"
  :author "Pavel Korolev"
  :license "MIT"
  :depends-on (alexandria vinoyaku/translator/yandex
                          vinoyaku/recognizer/tesseract
                          vinoyaku/analyzer/mecab
                          vinoyaku
                          bodge-utilities bodge-memory bodge-concurrency
                          glad-blob bodge-glad
                          bodge-host bodge-ui bodge-canvas bodge-canvas-ui
                          claw flexi-streams log4cl trivial-main-thread dissect opticl)
  :serial t
  :pathname "app/"
  :components ((:file "packages")
               (:file "preprocessing")
               (:file "ui-window")
               (:file "selection")
               (:file "selection-state")
               (:file "selection-window")
               (:file "main-window")
               (:file "application")))
