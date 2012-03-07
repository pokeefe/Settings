
;; Death to the tabs!  However, tabs historically indent to the next
;; 8-character offset; specifying anything else will cause *mass*
;; confusion, as it will change the appearance of every existing file.
;; In some cases (python), even worse -- it will change the semantics
;; (meaning) of the program.
;;
;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(setq auto-save-list-file-name nil) ;; don't be creatin' directories and cluttering my life!
(setq abbrev-file-name (concat user-emacs-directory "persistence/abbrev_defs"))
(setq transient-mark-mode t)
(setq sentence-end-double-space nil)
(setq delete-by-moving-to-trash t)
(setq shift-select-mode nil)
(setq whitespace-style '(trailing space-before-tab
                                  indentavindetion space-after-tab))
(setq whitespace-line-column 100)




;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(yas/hippie-try-expand
                                         try-expand-dabbrev
                                         try-expand-dabbrev-visible
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))


;; meaningful names for buffers with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-separator "/")
(setq uniquify-after-kill-buffer-p t)    ; rename after killing uniquified
(setq uniquify-ignore-buffers-re "^\\*") ; don't muck with special buffers


;; Open URLs in Google Chrome
(setq browse-url-browser-function 'browse-url-default-macosx-browser)


;; Transparently open compressed files. Damn cool.
(auto-compression-mode t)

;; Go directly to true file when following symlink
(setq vc-follow-symlinks t)

;; saveplace remembers your location in a file when saving files
(setq save-place-file (concat user-emacs-directory "persistence/saveplace"))
;; activate it for all buffers
(setq-default save-place t)
(require 'saveplace)


;; savehist keeps track of some history
(setq savehist-additional-variables
      ;; search entries
      '(search ring regexp-search-ring)
      ;; save every minute
      savehist-autosave-interval 60
      ;; keep the home clean
      savehist-file (concat user-emacs-directory "persistence/savehist"))
(savehist-mode t)


;; show-paren-mode: subtle highlighting of matching parens
(show-paren-mode t)
(setq show-paren-style 'parenthesis)

;; highlight the current line
(global-hl-line-mode t)

;; I work in camelCase all the time
(global-subword-mode t)


(require 'volatile-highlights)
(volatile-highlights-mode t)

(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
;; tramp, for sudo access
(require 'tramp)
;; keep in mind known issues with zsh - see emacs wiki
(setq tramp-default-method "ssh")
(setq tramp-auto-save-directory (concat user-emacs-directory "persistence/tramp.autosave"))
(setq tramp-backup-directory-alist
      `((".*" , (concat user-emacs-directory "persistence/tramp.backup") t)))



;; ido-mode, bless its heart
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-file-extensions-order '(".org" ".tex" ".m" ".txt")
      ido-save-directory-list-file (concat user-emacs-directory "persistence/ido.last"))


;; save recent files (courtesy of virtual buffers in ido)
(setq recentf-save-file (concat user-emacs-directory "persistence/recentf")
      recentf-max-saved-items 200
      recentf-max-menu-items 15)


(set-default 'imenu-auto-rescan t)

(setq smex-save-file (concat user-emacs-directory "persistence/smex-items"))
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)


(add-to-list 'load-path (concat user-emacs-directory "/vendor/nav"))
(require 'nav)


;; So that new files opened in terminal do not spawn a new window
(setq ns-pop-up-frames nil)


;; flyspell-mode does spell-checking on the fly as you type
(setq ispell-program-name "aspell" ; use aspell instead of ispell
      ispell-extra-args '("--sug-mode=ultra"))
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checker." t)

(defun turn-on-flyspell ()
  "Force flyspell-mode on using a positive argument.  For use in hooks."
  (interactive)
  (flyspell-mode +1))

(add-hook 'message-mode-hook 'turn-on-flyspell)
(add-hook 'text-mode-hook 'turn-on-flyspell)

;; enable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; enabled change region case commands
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(require 'expand-region)
(global-set-key (kbd "C-@") 'er/expand-region)


;; bookmarks
(setq bookmark-default-file (concat user-emacs-directory "persistence/bookmarks")
      bookmark-save-flag 1)

;; Good number for MacBook Pro half screen width
(setq default-fill-column 90)

;; enabled auto-fill mode in text-mode and all related modes
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; load yasnippet
(require 'yasnippet)
(yas/initialize)
(setq yas/indent-line 'auto)
(setq yas/wrap-around-region 'cua) ;; This isn't really working!
(yas/load-directory (concat user-emacs-directory "snippets"))

;; Sweet auto pairing
(require 'autopair)
(autopair-global-mode)
(setq autopair-autowrap t)


(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
(setq kill-buffer-query-functions
      (remove 'process-kill-buffer-query-function
              kill-buffer-query-functions))


;; ediff - don't start another frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(prefer-coding-system 'utf-8)
(setq message-log-max 500)
(server-start)

(provide 'init-editing)
