;;; Things that don't fit anywhere else
(when window-system
  (mouse-wheel-mode t)
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (blink-cursor-mode -1))

;; can't do it at launch or emacsclient won't always honor it
(add-hook 'before-make-frame-hook 'turn-off-tool-bar)


(setq visible-bell t
      font-lock-maximum-decoration t
      inhibit-startup-message t
      transient-mark-mode t
      sentence-end-double-space nil
      color-theme-is-global t
      delete-by-moving-to-trash t
      shift-select-mode nil
      truncate-partial-width-windows nil
      uniquify-buffer-name-style 'forward
      whitespace-style '(trailing space-before-tab
                                  indentavindetion space-after-tab)
      whitespace-line-column 100
      ediff-window-setup-function 'ediff-setup-windows-plain
      delete-selection-mode t
      diff-switches "-u"
      save-place-file (concat dotfiles-dir "vendor/places"))



;; Don't clutter up directories with files~
(setq backup-directory-alist `(("." . ,(expand-file-name
                                        (concat dotfiles-dir "backups")))))


;; Transparently open compressed files. Damn cool.
(auto-compression-mode t)

;; Open URLs in Google Chrome
(setq browse-url-browser-function 'browse-url-default-macosx-browser)

;; Listen, I like the menu bar...
(menu-bar-mode 1)

;; Save a list of recent files visited.
;; (recentf-mode 1)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Who would want otherwise?
(global-auto-revert-mode t)

;; Bless it's heart
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)


(setq ido-file-extensions-order '(".org" ".tex" ".m" ".txt"))

;; So that new files opened in terminal do not spawn a new window
(setq ns-pop-up-frames nil)

(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(setq message-log-max 500)


(set-default 'indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(prefer-coding-system 'utf-8)

(server-start)

(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)
(setq kill-buffer-query-functions
      (remove 'process-kill-buffer-query-function
              kill-buffer-query-functions))

(setq vc-follow-symlinks nil)

;; Good number for MacBook Pro half screen width
(setq default-fill-column 90)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


(defalias 'yes-or-no-p 'y-or-n-p)
(random t) ;; Seed the random-number generator

;; Hippie expand: at times perhaps too hip
(delete 'try-expand-line hippie-expand-try-functions-list)
(delete 'try-expand-list hippie-expand-try-functions-list)


;; Associate modes with file extensions
(add-to-list 'auto-mode-alist '("COMMIT_EDITMSG$" . diff-mode))
(add-to-list 'auto-mode-alist '("\\.css$" . css-mode))
(add-to-list 'auto-mode-alist '("\\.rb$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.js\\(on\\)?$" . javascript-mode))

(add-to-list 'auto-mode-alist '("\\.xml$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.html$" . html-mode))
(add-to-list 'auto-mode-alist '("\\.rhtml$" . html-mode))

;; Cosmetics
(eval-after-load 'diff-mode
  '(progn
     (set-face-foreground 'diff-added "green4")
     (set-face-foreground 'diff-removed "red3")))


;; Save backups in one place
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir
  (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

(provide 'miscInit)
