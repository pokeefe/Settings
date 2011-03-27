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
(load-file "~/.emacs.d/lisp/color-theme-cobalt.el")
(color-theme-cobalt)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  Useful Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; common lisp extensions
(require 'cl) 
	
;; saves position in buffers between opens
(require 'saveplace) 
	
;; better way to make buffer names unique	
(require 'uniquify)

; color stuff
(require 'ansi-color)

;; a menu of recently opened files
(require 'recentf)

;; Allows you to find unbound key combinations
(require 'unbound)

;; Load up ELPA, the package manager:
(require 'package)
(package-initialize)

;; Sweet autopairing
(require 'autopair)
(autopair-global-mode)

;; Snippets
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet.el"))
(require 'yasnippet)
(yas/initialize)
(setq yas/indent-line 'fixed) 
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
(global-set-key [?\A- ] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        ))


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



;; Benchmarking
(message "My .emacs loaded in %ds"
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*) (second
                                                       *emacs-load-start*)))))


(provide 'init)