(uiop:define-package :vinoyaku.app.histogram
  (:use :cl :bodge-math)
  (:export #:histogram
           #:update-histogram-array
           #:histogram-threshold
           #:histogram-bounds))

(uiop:define-package :vinoyaku.app
  (:use :cl :bodge-math :vinoyaku.app.histogram)
  (:export #:run))
