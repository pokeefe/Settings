;;; init.el --- Where all the magic begins
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  This should all be perfectly platform agnostic!
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Benchmarking
(defvar *emacs-load-start* (current-time))


;; Load path etc:
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))
(add-to-list 'load-path (concat dotfiles-dir "/lisp"))
(setq autoload-file (concat dotfiles-dir "/lisp/loaddefs.el"))
(setq package-user-dir (concat dotfiles-dir "elpa"))

;; Keep customizations in a separate file
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

;; Color Theme of Choice
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(require 'color-theme)
(load-file "~/.emacs.d/lisp/color-theme-tangotango.el")
(color-theme-tangotango)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Useful Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; common lisp extensions
(require 'cl)

;; Smooth-scrolling mode. Thanks Paul.
(require 'smooth-scrolling)

;; saves position in buffers between opens
(require 'saveplace)

;; better way to make buffer names unique
(require 'uniquify)

                                        ; color stuff
(require 'ansi-color)

;; a menu of recently opened files
(require 'recentf)
(setq recentf-save-file (concat dotfiles-dir "vendor/.recentf"))
(setq recentf-auto-cleanup 'never) ;; This fixes a bug that causes Tramp to block emacs at very inopportune times.
(recentf-mode 1)

(global-whitespace-mode)
(global-hl-line-mode)
(global-subword-mode t)

;; ido-mode is loaded elsewhere (I should organize all this soon), but I have seeing a
;; .ido.last in my home director
(setq ido-save-directory-list-file (concat dotfiles-dir "vendor/.ido.last"))


;; Allows you to find unbound key combinations
(require 'unbound)

;; Load up ELPA, the package manager:
(require 'package)
(package-initialize)

;; Sweet autopairing
(require 'autopair)
(autopair-global-mode)

;; Unlike linum, this places current line and column info right above minibuffer
(require 'line-num)

;; Let's get some real objective-c mode stuff going on here
(require 'objc-c-mode)

;; Snippets
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet.el"))
(require 'yasnippet)
(yas/initialize)
(setq yas/indent-line 'auto)
(setq yas/wrap-around-region 'cua) ;; This isn't really working!
(yas/load-directory (concat dotfiles-dir "/vendor/yasnippet.el/snippets"))


;; Sweet textmate mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/textmate.el"))
(require 'textmate)
(require 'textmateExtension)
(textmate-mode)

;; for OCD programmers like myself
(require 'whitespace)

;; need me some linum
(require 'linum)
(global-linum-mode 1)
(setq linum-format 'dynamic)

;; Make hippie expand work nicely with yasnippet
(require 'hippie-exp)
(when (string-equal system-type "darwin")
  (global-set-key [?\A- ] 'hippie-expand))
(when (string-equal system-type "gnu/linux")
  (global-set-key [?\A- ] 'hippie-expand))
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        ))

;; Markdown mode
(autoload 'markdown-mode "markdown-mode.el"
   "Major mode for editing Markdown files" t)
(setq auto-mode-alist
      (cons '("\\.md" . markdown-mode) auto-mode-alist))

;; Lua mode
(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))


;; Paul says Dired is life changing. Let's try it
(require 'dired+)

;; Calendar Framework
;; (add-to-list 'load-path (concat dotfiles-dir "/vendor/calfw"))
;; (require 'calfw)
;; (require 'calfw-ical)
;; (cfw:open-ical-calendar "https://www.google.com/calendar/ical/patokeefe1%40gmail.com/private-e8052c409c33af06134a40b7ad824f78/basic.ics")
;; (global-set-key [(control c) (control c)] 'cfw:open-ical-calendar)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Custom Lisp Inits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'keyboardBindings)
(require 'customFunctions)
(require 'miscInit)
(require 'plainTextAdditions)
(require 'registers)
(require 'lispHelpers)
(require 'applescript)
(require 'orgAdditions)
(require 'xcodeHelper)
(require 'flymake)
(require 'latexInit)
(require 'matlabInit)
(require 'dired-mod)

(set-frame-size-according-to-resolution)

;; Benchmarking
(message "My .emacs loaded in %ds"
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*) (second
                                                       *emacs-load-start*)))))

(open-filelist '("~/.emacs.d/init.el"
                 "~/Dropbox/Org/gtd.org"))

;; (dired (getenv "HOME"))
;; (switch-to-buffer (user-login-name))
;; (split-window-horizontally)
(switch-to-buffer "gtd.org")


(provide 'init)
