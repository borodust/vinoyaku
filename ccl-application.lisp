(in-package :vinoyaku)


(alexandria:define-constant +channels+ 4)


(defmacro @ (name)
    `(find-class ',name))


(defun byte-size-of (foreign-type)
  (/ (ccl::foreign-type-bits
      (ccl::parse-foreign-type foreign-type))
     8))


(defun ns-app ()
  (#/sharedApplication (find-class 'ns:ns-application)))


(defmacro do-ns-array ((obj array) &body body)
  (once-only (array)
    (with-gensyms (idx)
      `(loop for ,idx from 0 below (#/count ,array)
          for ,obj = (#/objectAtIndex: ,array ,idx)
          do (progn ,@body)))))


(defun map-ns-array (mapper ns-array)
  (loop for idx from 0 below (#/count ns-array)
     collect (funcall mapper (#/objectAtIndex: ns-array idx))))


(defun draw-bitmap (image-ref image)
  (ccl:rlet ((rect #>NSRect))
    (let* ((color-space (#_CGColorSpaceCreateDeviceRGB))
           (width (#_CGImageGetWidth image-ref))
           (height (#_CGImageGetHeight image-ref)))
      (ns:init-ns-rect rect 0 0 width height)
      (unwind-protect
           (let* ((bits-per-component 8)
                  (bytes-per-row (* (foreign-image-channels image) width))
                  (opts (logior #$kCGImageAlphaPremultipliedLast
                                #$kCGBitmapByteOrder32Big))
                  (bitmap-context (#_CGBitmapContextCreate (foreign-image-ptr image)
                                                           width height
                                                           bits-per-component bytes-per-row
                                                           color-space
                                                           opts)))
             (unwind-protect
                  (#_CGContextDrawImage bitmap-context rect image-ref)
               (#_CGContextRelease bitmap-context)))
        (#_CGColorSpaceRelease color-space)))))


(defun read-screen (image x y)
  (ccl:rlet ((rect #>NSRect))
    (ns:init-ns-rect rect x y (foreign-image-width image) (foreign-image-height image))
    (let ((image-ref (#_CGDisplayCreateImageForRect (#_CGMainDisplayID) rect)))
      (draw-bitmap image-ref image)))
  nil)


(defun find-view (root-view test-fn)
  (if-let ((result (funcall test-fn root-view)))
    result
    (do-ns-array (subview (#/subviews root-view))
      (when-let ((result (find-view subview test-fn)))
        (return result)))))


(defun find-explanation-area (root-view)
  (find-view root-view
             (lambda (view)
               (let ((identifier (#/identifier view)))
                 (when (#/isEqualToString: #@"vnyk-explanation-area" identifier)
                   view)))))


(defun initialize-application ()
  (log:debug "Initializing NSApp")
  (ns-app)
  (#/setActivationPolicy: (ns-app) #$NSApplicationActivationPolicyRegular)
  (#/activateIgnoringOtherApps: (ns-app) #$YES))


(defun load-bundle ()
  (let* ((bundle (#/mainBundle (@ ns:ns-bundle)))
         (array-ptr (#_malloc (byte-size-of :address))))
    (unwind-protect
         (progn
           (#/loadNibNamed:owner:topLevelObjects: bundle
                                                  #@"MainMenu"
                                                  (ns-app)
                                                  array-ptr)
           (#/loadNibNamed:owner:topLevelObjects: bundle
                                                  #@"MainWindow"
                                                  (ns-app)
                                                  array-ptr)
           (#/loadNibNamed:owner:topLevelObjects: bundle
                                                  #@"CapturePanel"
                                                  (ns-app)
                                                  array-ptr))
      (#_free array-ptr))
    (log:debug "Bundle loaded: ~A:" (#/bundlePath bundle))
    bundle))


(defun windows ()
  (flet ((name-window-pair (win)
           (let ((identifier (#/identifier win)))
             (unless (or (null identifier) (ccl:%null-ptr-p identifier))
               (list (switch (identifier :test #'#/isEqualToString:)
                       (#@"vnyk-main-window" :main-window)
                       (#@"vnyk-capture-panel" :capture-panel))
                     win)))))
    (reduce #'nconc (map-ns-array #'name-window-pair (#/windows (ns-app))))))


(defparameter *behavior-controller* nil)


(defclass behavior-controller (ns:ns-object)
  ((text-consumer :initarg :text-consumer)
   (rect-provider :initarg :rect-provider)
   (context :initform (make-context)))
  (:metaclass ns:+ns-object))


(objc:defmethod ("explain:" :void) ((self behavior-controller) (sender :id))
  (declare (ignore sender))
  (log:debug "Explanation requested")
  (with-slots (text-consumer rect-provider context) self
    (block continue
      (handler-bind ((t (lambda (e)
                          (log:error "Failed to explain: ~A" (dissect:present e))
                          (return-from continue))))
        (destructuring-bind (x y width height) (funcall rect-provider)
          (recognizr:with-foreign-image (image width height)
            (read-screen image x y)
            (let ((opticl-image (recognizr:foreign-image->opticl-image image)))
              (funcall text-consumer (explain context opticl-image)))))))))


(defclass capture-responder (ns:ns-responder)
  ((capture-panel :initarg :capture-panel)
   (rect-consumer :initarg :rect-consumer))
  (:metaclass ns:+ns-object))


(defun invert-y (y height)
  (let ((screen-height (ns:ns-rect-height (#/frame (#/mainScreen (@ ns:ns-screen))))))
    (- screen-height (+ y height))))


(objc:defmethod (#/keyDown: :void) ((self capture-responder) (event :id))
  (with-slots (capture-panel rect-consumer) self
    (let* ((content-frame (#/contentRectForFrameRect: capture-panel (#/frame capture-panel)))

           (coords (list (ns:ns-rect-x content-frame)
                         (invert-y (ns:ns-rect-y content-frame)
                                   (ns:ns-rect-height content-frame))
                         (ns:ns-rect-width content-frame)
                         (ns:ns-rect-height content-frame))))
      (switch ((#/keyCode event) :test #'=)
        (36 (funcall rect-consumer coords)
            (#/orderOut: capture-panel nil))
        (53 (#/orderOut: capture-panel nil))
        (t (log:debug "~A: ~A" coords (#/keyCode event))))))
  (call-next-method))


(objc:defmethod ("enterCaptureMode:" :void) ((self capture-responder) (sender :id))
  (with-slots (capture-panel) self
    (#/makeKeyAndOrderFront: capture-panel sender)
    #++(#/runModalForWindow: (ns-app) capture-panel)))


(defvar *capture-responder* nil)


(defun find-control-view (win)
  (first (map-ns-array #'#/view (#/items (#/toolbar win)))))


(defun setup-behavior ()
  (destructuring-bind (&key main-window capture-panel &allow-other-keys) (windows)
    (when-let ((area (find-explanation-area (#/contentView main-window))))
      (let* ((control-view (find-control-view main-window))
             (explain-button (#/viewWithTag: control-view 0))
             (snap-button (#/viewWithTag: control-view 1))
             (x-input (#/viewWithTag: control-view 2))
             (y-input (#/viewWithTag: control-view 3))
             (w-input (#/viewWithTag: control-view 4))
             (h-input (#/viewWithTag: control-view 5)))
        (flet ((apply-rect (rect)
                 (log:debug "Applying rect: ~A" rect)
                 (destructuring-bind (x y w h) rect
                   (#/setIntValue: x-input (floor x))
                   (#/setIntValue: y-input (floor y))
                   (#/setIntValue: w-input (floor w))
                   (#/setIntValue: h-input (floor h))))
               (extract-rect ()
                 (list (#/intValue x-input)
                       (#/intValue y-input)
                       (#/intValue w-input)
                       (#/intValue h-input)))
               (display-explanation (string)
                 (log:debug "Displaying explanation: ~A:" string)
                 (objc:with-autoreleased-nsstrings ((ns-string string))
                   (ccl:rlet ((range #>NSRange))
                     (ns:init-ns-range range 0 (#/length (#/attributedString area)))
                     (#/insertText:replacementRange: area ns-string range)))))
        (setf *behavior-controller* (make-instance 'behavior-controller
                                                   :text-consumer #'display-explanation
                                                   :rect-provider #'extract-rect)
              *capture-responder* (make-instance 'capture-responder
                                                 :capture-panel capture-panel
                                                 :rect-consumer #'apply-rect))

        (#/setNextResponder: *capture-responder* (#/firstResponder capture-panel))
        (unless (#/makeFirstResponder: capture-panel *capture-responder*)
          (log:error "Couldn't update first responder for capture window"))

        (dolist (btn (list #$NSWindowCloseButton
                           #$NSWindowMiniaturizeButton
                           #$NSWindowZoomButton))
          (#/setHidden: (#/standardWindowButton: capture-panel btn) #$YES))

        #++(let ((main-screen-frame (#/frame (#/mainScreen (@ ns:ns-screen)))))
             (#/setFrame:display: capture-panel main-screen-frame #$NO))
        (#/setOpaque: capture-panel #$NO)

        (#/setTarget: explain-button *behavior-controller*)
        (#/setAction: explain-button (objc:@selector "explain:"))

        (#/setTarget: snap-button *capture-responder*)
        (#/setAction: snap-button (objc:@selector "enterCaptureMode:")))))))


(defun main (&key (blocking-p t))
  (log:config :sane2 :debug)
  (with-body-in-main-thread (:blocking blocking-p)
    (log:debug "Starting GUI application")
    (handler-case
        (objc:with-autorelease-pool
          (initialize-application)
          (load-bundle)
          (setup-behavior)

          (log:debug "Application initialized. Starting main loop")
          (#/run (ns-app))
          (log:debug "GUI Application exiting"))
      (t (e) (log:error "Error during app init: ~A" e)))))
