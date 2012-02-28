;;; init.el --- Where all the magic begins
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"



;; Benchmarking
(defvar *emacs-load-start* (current-time))

  (dolist (mode '(tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1)))



;; Load path setup
(setq dotfiles-dir (file-name-directory
                    (or (buffer-file-name) load-file-name)))
(add-to-list 'load-path dotfiles-dir)
(add-to-list 'load-path (concat dotfiles-dir "/vendor"))
(add-to-list 'load-path (concat dotfiles-dir "/lisp"))
(add-to-list 'load-path (concat dotfiles-dir "/elpa"))
(setq autoload-file (concat dotfiles-dir "/lisp/loaddefs.el"))


;;;;;;;;;;;;;
;;; Package interface
;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:
(defvar my-packages '(smex ido-ubiquitous idle-highlight-mode expand-region)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(pending-delete-mode 'nil)
(setq next-line-add-newlines t)
(setq scroll-preserve-screen-position t)







;; Keep customizations in a separate file
(setq custom-file (concat dotfiles-dir "custom.el"))
(load custom-file 'noerror)

;; Color Theme of Choice
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(require 'color-theme)
;; (load-file "~/.emacs.d/vendor/color-theme/themes/color-theme-tangotango.el")
;; (color-theme-tangotango)
(load-file "~/.emacs.d/vendor/color-theme/themes/naquadah-theme.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Useful Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Smooth-scrolling mode. Thanks Paul.
(require 'smooth-scrolling)

;; saves position in buffers between opens
(require 'saveplace)

;; better way to make buffer names unique
(require 'uniquify)

; color stuff
(require 'ansi-color)

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)

;; for OCD programmers like myself
(require 'whitespace)

(global-hl-line-mode)

(global-subword-mode t)

(add-to-list 'load-path (concat dotfiles-dir "/vendor/nav"))
(require 'nav)

;; ido-mode is loaded elsewhere (I should organize all this soon), but I have seeing a
;; .ido.last in my home director
(setq ido-save-directory-list-file (concat dotfiles-dir "vendor/.ido.last"))


;; Sweet autopairing
(add-to-list 'load-path (concat dotfiles-dir "/vendor/autopair"))
(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)

;; Let's get some real objective-c mode stuff going on here
(require 'objc-c-mode)

;; Snippets
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet.el"))
(require 'yasnippet)
(yas/initialize)
(setq yas/indent-line 'auto)
(setq yas/wrap-around-region 'cua) ;; This isn't really working!
(yas/load-directory (concat dotfiles-dir "/vendor/yasnippet.el/snippets"))


;; need me some linum
(require 'linum)
(global-linum-mode 1)
(setq linum-format 'dynamic)

;; Unlike linum, this places current line and column info right above minibuffer
(require 'line-num)


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
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Custom Lisp Inits
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(require 'customFunctions)
(require 'miscInit)
(require 'plainTextAdditions)
(require 'registers)
(require 'lispHelpers)
(require 'orgAdditions)
(require 'flymake)
(require 'latexInit)
(require 'matlabInit)
(require 'dired-mod)

(define-comment-line-textmate)

(require 'keyboardBindings)

(set-frame-size-according-to-resolution)

;; Benchmarking
(message "My .emacs loaded in %ds"
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*) (second
                                                       *emacs-load-start*)))))

(open-filelist '("~/.emacs.d/init.el"
                 "~/Dropbox/Org/gtd.org"))

(switch-to-buffer "gtd.org")

(provide 'init)
