(in-package :vinoyaku)


(defmacro @ (name)
    `(find-class ',name))


(defun ns-app ()
  (#/sharedApplication (find-class 'ns:ns-application)))


(defun main (&key (blocking-p t))
  (log:config :sane2)
  (with-body-in-main-thread (:blocking blocking-p)
    (log:info "Starting GUI application")
    (handler-case
        (objc:with-autorelease-pool
          (log:debug "Initializing NSApp")
          (ns-app)
          (log:debug "Filling UI")
          (objc:with-autoreleased-nsstrings ((quit-title "Quit")
                                             (q-hotkey "q"))
            (let* ((menu-bar (#/new (@ ns:ns-menu)))
                   (app-menu-bar-item (#/new (@ ns:ns-menu-item)))
                   (app-menu (#/new (@ ns:ns-menu)))
                   (quit-menu-item (#/initWithTitle:action:keyEquivalent:
                                    (#/alloc (@ ns:ns-menu-item))
                                    quit-title
                                    (objc:@selector #/terminate:)
                                    q-hotkey)))
              (#/setMainMenu: (ns-app) menu-bar)

              (#/addItem: menu-bar app-menu-bar-item)
              (#/setSubmenu: app-menu-bar-item app-menu)
              (#/addItem: app-menu quit-menu-item)))

          (ccl:rlet ((content-rect #>NSRect)
                     (win-origin #>NSPoint))
            (ns:init-ns-rect content-rect 0d0 0d0 300d0 300d0)
            (ns:init-ns-point win-origin 20d0 20d0)
            (log:debug "Creating main window")
            (let* ((win (#/initWithContentRect:styleMask:backing:defer:
                         (#/alloc (@ ns:ns-window))
                         content-rect
                         (logior #$NSTitledWindowMask
                                 #$NSClosableWindowMask
                                 #$NSMiniaturizableWindowMask
                                 #$NSResizableWindowMask)
                         #$NSBackingStoreBuffered
                         #$NO)))
              (objc:with-autoreleased-nsstrings ((title "VINOYAKU"))
                (#/setTitle: win title))
              (#/makeKeyAndOrderFront: win nil)
              (#/cascadeTopLeftFromPoint: win win-origin)))


          (#/setActivationPolicy: (ns-app) #$NSApplicationActivationPolicyRegular)
          (#/activateIgnoringOtherApps: (ns-app) #$YES)
          (log:info "Application initialized. Starting main loop")
          (#/run (ns-app))
          (log:info "GUI Application exiting"))
      (t (e) (log:error "Error during app init: ~A" e)))))
