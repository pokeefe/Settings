;;; init.el --- Where all the magic begins
;;
;; "Emacs outshines all other editing software in approximately the
;; same way that the noonday sun does the stars. It is not just bigger
;; and brighter; it simply makes everything else vanish."
;; -Neal Stephenson, "In the Beginning was the Command Line"

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
(setq custom-file (concat dotfiles-dir "custom.el"))

(setq exec-path (append exec-path '("/opt/local/bin/gs")))


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


;; These should be loaded on startup rather than autoloaded on demand
;; since they are likely to be used in every session:

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

;; many keyboard shortcuts
(require 'keyboardBindings)


(require 'customFunctions)
(require 'miscInit)
(require 'plainTextAdditions)
(require 'registers) ;; quick jump-to-file capabilities
(require 'lispHelpers)
(require 'applescript)
(require 'orgAdditions)
(require 'xcodeHelper)

(load custom-file 'noerror)


;; Snippets
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet.el"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "/vendor/yasnippet.el/snippets"))


;; Sweet autopairing
(require 'autopair)
(autopair-global-mode) 

;; Sweet textmate mode
(add-to-list 'load-path (concat dotfiles-dir "/vendor/textmate.el"))
(require 'textmate)
(require 'peepopen)
(require 'textmateExtension)
(textmate-mode)
(setq ns-pop-up-frames nil)

;; for OCD programmers like myself
(require 'whitespace)

;; need me some linum
(require 'linum)
(global-linum-mode 1)
(setq linum-format "%4d ")

;; Remove scrollbars and make hippie expand
;; work nicely with yasnippet
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(require 'hippie-exp)
(global-set-key [?\A- ] 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        ;;        try-expand-dabbrev-from-kill
        ;;         try-complete-file-name
        ;;         try-complete-file-name-partially
        ;;         try-complete-lisp-symbol
        ;;         try-complete-lisp-symbol-partially
        ;;         try-expand-line
        ;;         try-expand-line-all-buffers
        ;;         try-expand-list
        ;;         try-expand-list-all-buffers
        ;;        try-expand-whole-kill
        ))

(defun indent-or-complete ()
  (interactive)
  (if (and (looking-at "$") (not (looking-back "^\\s-*")))
      (hippie-expand nil)
    (indent-for-tab-command)))
(add-hook 'find-file-hooks (function (lambda ()
                                       (local-set-key (kbd "TAB") 'indent-or-complete))))


;; Color Themes
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(require 'color-theme)
;; (color-theme-initialize)
(load-file "~/.emacs.d/lisp/color-theme-cobalt-two.el")
(color-theme-cobalt-two)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq LaTeX-command "latex -shell-escape")

(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(load "preview-latex.el" nil t t)

;; apply LaTeX hooks (spellcheck, etc.)
(add-hook 'LaTeX-mode-hook (lambda ()
			     (flyspell-mode)
			     (outline-minor-mode)
			     (auto-fill-mode)))
			
;; assist syncTeX			
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)

;; use Skim to open PDFs
(add-hook 'LaTeX-mode-hook
     (lambda()
       (add-to-list 'TeX-expand-list
            '("%q" skim-make-url))))

(defun skim-make-url () (concat
       (TeX-current-line)
       " "
       (expand-file-name (funcall file (TeX-output-extension) t)
           (file-name-directory (TeX-master-file)))
       " "
       (buffer-file-name)))

(setq TeX-view-program-list
     '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline %q")))

(setq TeX-view-program-selection '((output-pdf "Skim")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matlab Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
(load-file "~/.emacs.d/vendor/cedet/common/cedet.el")


(add-to-list 'load-path "~/.emacs.d/vendor/matlab-emacs")
  (require 'matlab-load)

;; Enable EDE (Project Management) features
; (global-ede-mode 1)

;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")


;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
; (semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
; (semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
;; (semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberent ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languges only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
;; (global-srecode-minor-mode 1)


;; Benchmarking
(message "My .emacs loaded in %ds"
         (destructuring-bind (hi lo ms) (current-time)
           (- (+ hi lo) (+ (first *emacs-load-start*) (second
                                                       *emacs-load-start*)))))


(provide 'init)