;;
;; UI
;;


;; Remove the tool-bar and the scroll-bars
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Fonts
(when (string-equal system-type "gnu/linux")
  ;; Linux font options. This closely resembles Menlo. Might be a good
  ;; idea to change to something
  (set-frame-font "DejaVu Sans Mono")
  (set-face-attribute 'default nil :height 100))

(when (string-equal system-type "darwin")
  (normal-erase-is-backspace-mode 1)
    ;;; Set font easily, if Emacs 23 (Cocoa)
  (if (>= emacs-major-version 23)
      (set-frame-font "Menlo-12")))



;; the menu bar is mostly useless as well
;; but removing it under OS X doesn't make much sense
(defun init-frame-config (frame)
  "Custom behaviors for new frames."
  (if (eq system-type 'darwin)
      (with-selected-frame frame
        (if (display-graphic-p)
            (modify-frame-parameters frame '((menu-bar-lines . 1)))
          (modify-frame-parameters frame '((menu-bar-lines . 0)))))
    (menu-bar-mode -1)))

;; run now
(init-frame-config (selected-frame))
;; and later
(add-hook 'after-make-frame-functions 'init-frame-config)



;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable startup screen
(setq inhibit-startup-screen t)

(when window-system
  (mouse-wheel-mode t)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1))


(setq visible-bell t)
(setq font-lock-maximum-decoration t)
(setq color-theme-is-global t)
(setq truncate-partial-width-windows nil)


;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)


;; Smooth-scrolling mode. Thanks Paul.
(require 'smooth-scrolling)


;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; need me some customized linum
(require 'linum)
(global-linum-mode 1)
(setq linum-format 'dynamic)


;; Cosmetics
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))


;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; custom Emacs 24 color themes support
(add-to-list 'custom-theme-load-path (concat dotfiles-dir "themes/"))

;; Use zenburn as the theme
;; (load-theme 'zenburn t)

;; Or this modified tango tango theme
(require 'color-theme)
(load-file "~/.emacs.d/themes/naquadah-theme.el")


(provide 'init-ui)
