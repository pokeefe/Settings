;;
;; Where it all starts
;;
;; My init, like most I've seen, is a giant mashup of things I've found on the Internets.
;; Check out the emacs-starter-kit and emacs-prelude for more inspiration.
;;

(require 'cl)

;; Benchmarking
(defvar *emacs-load-start* (current-time))

;; The one UI change I'll do outside of init-ui.el
;; Needs to be done early so window resizing works properly
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))


;; Some convenient variables
(defvar dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name))
  "The root directory of the Emacs initialization process.")
(defvar modules-dir (concat dotfiles-dir "modules/")
  "This directory contains most of the customizations. These files cover editing,
UI, and mode specific changes.")
(defvar vendor-dir (concat dotfiles-dir "vendor/")
  "This directory house Emacs Lisp packages that are not yet available in
ELPA (or Marmalade).")

;; add directories to Emacs's `load-path'
(add-to-list 'load-path modules-dir)
(add-to-list 'load-path vendor-dir)

;; Keep customizations in a separate file
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)


;; Core customizations
(require 'init-packages)
(require 'init-ui)
(require 'init-functions)
(require 'init-editing)
(require 'init-registers)
(require 'init-keybindings)


;; Mode specific customizations
(require 'init-programming)
(require 'init-lisp)
(require 'init-org)
(require 'init-flymake)
(require 'init-latex)
(require 'init-matlab)
(require 'init-python)

(set-frame-size-according-to-resolution)

(open-filelist '("~/.emacs.d/init.el"
                 "~/Dropbox/Org/gtd.org"))

(switch-to-buffer "gtd.org")

;; Benchmarking
(message "My .emacs loaded in %ds"
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*) (second
                                                       *emacs-load-start*)))))

(provide 'init)
