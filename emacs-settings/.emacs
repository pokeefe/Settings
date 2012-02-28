;;; Platform specific initializations.

(when (string-equal system-type "gnu/linux")
  (message "Linux Initialization")

  (add-to-list 'load-path "~/.emacs.d/")
  (load-file "~/.emacs.d/init.el")

  ;; Linux font options. This closely resembles Menlo. Might be a good
  ;; idea to change to something
  (set-frame-font "DejaVu Sans Mono")
  (set-face-attribute 'default nil :height 100)
  )

(when (string-equal system-type "darwin")
  (message "Mac OS X Initialization")
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
 ; (add-to-list 'load-path "~/Desktop/emacs-prelude/")
  ;(load-file "~/Desktop/emacs-prelude/init.el")
  (let ((path))
    (setq path (concat "/opt/local/bin:"
                       "/opt/local/sbin:"
                       "/usr/bin:"
                       "/bin:"
                       "/usr/sbin:"
                       "/sbin:"
                       "/usr/local/bin:"
                       "/usr/texbin:"
                       "/usr/X11/bin:"))
                        (setenv "PATH" path))
  (add-to-list 'org-modules 'org-mac-iCal)
  )



(put 'dired-find-alternate-file 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
