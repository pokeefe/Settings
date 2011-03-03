;;; Platform specific calls. Everything in .emacs.d should be platform agnostic.

(when (string-equal system-type "gnu/linux")
  (message "linux load time")
  )

(when (string-equal system-type "darwin")
(message "mac os x load time")
  (normal-erase-is-backspace-mode 1)
    ;;; Set font easily, if Emacs 23 (Cocoa)
    (if (>= emacs-major-version 23)
        (set-frame-font "Menlo-12"))
    ;;; I prefer cmd key for meta
    (setq mac-option-key-is-meta nil)
    (setq mac-command-key-is-meta t)
    (setq mac-command-modifier 'meta)
    (setq mac-option-modifier 'alt)
    (add-to-list 'load-path "~/.emacs.d/")
    (load-file "~/.emacs.d/init.el")
  )



