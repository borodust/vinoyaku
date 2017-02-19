(in-package :vinoyaku.def)


(defpackage :vinoyaku
  (:nicknames :vnyk)
  (:use :cl :alexandria :plus-c :flexi-streams :trivial-main-thread :recognizr)
  (:export make-context
           explain))
