(cl:defpackage :vinoyaku.app.histogram
  (:use :cl :bodge-math)
  (:export #:histogram
           #:histogram-array))

(cl:defpackage :vinoyaku.app
  (:use :cl :bodge-math :vinoyaku.app.histogram)
  (:export #:run))
