;;; customFunctions.el --- Define some custom functions


(defun esk-local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))

(defun esk-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))

(defun esk-turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))

(defun esk-turn-on-save-place-mode ()
  (require 'saveplace)
  (setq save-place t))

(defun esk-turn-on-whitespace ()
  (whitespace-mode t))


(defun esk-turn-on-idle-highlight-mode ()
  (idle-highlight-mode t))

(defun esk-pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun esk-add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\|NOCOMMIT\\)"
          1 font-lock-warning-face t))))

(add-hook 'prog-mode-hook 'esk-local-column-number-mode)
(add-hook 'prog-mode-hook 'esk-local-comment-auto-fill)
(add-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)
(add-hook 'prog-mode-hook 'esk-turn-on-save-place-mode)
(add-hook 'prog-mode-hook 'esk-pretty-lambdas)
(add-hook 'prog-mode-hook 'esk-add-watchwords)
(add-hook 'prog-mode-hook 'esk-turn-on-whitespace)
(add-hook 'prog-mode-hook 'esk-turn-on-idle-highlight-mode)

(defun esk-prog-mode-hook ()
  (run-hooks 'prog-mode-hook))

(defun turn-off-tool-bar ()
  (tool-bar-mode -1))



(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
      (progn
        (add-to-list 'default-frame-alist
                     (cons 'height (/ (- (x-display-pixel-height) 50)
                                      (frame-char-height))))

        (add-to-list 'default-frame-alist
                     (cons 'width (/ (/ (- (x-display-pixel-width) 0)
                                        (frame-char-width)) 2))))))

;; Thanks Paul
(defun open-filelist (fileList)
  (while fileList
    (let ((currentFile (car fileList)))
      (if (file-exists-p currentFile)
          (find-file (file-truename currentFile))))
    (setq fileList (cdr fileList))))


;; Buffer-related
(defun ido-imenu ()
  "Update the imenu index and then use ido to select a symbol to navigate to."
  (interactive)
  (imenu--make-index-alist)
  (let ((name-and-pos '())
        (symbol-names '()))
    (flet ((addsymbols (symbol-list)
                       (when (listp symbol-list)
                         (dolist (symbol symbol-list)
                           (let ((name nil) (position nil))
                             (cond
                              ((and (listp symbol) (imenu--subalist-p symbol))
                               (addsymbols symbol))

                              ((listp symbol)
                               (setq name (car symbol))
                               (setq position (cdr symbol)))

                              ((stringp symbol)
                               (setq name symbol)
                               (setq position (get-text-property 1 'org-imenu-marker symbol))))

                             (unless (or (null position) (null name))
                               (add-to-list 'symbol-names name)
                               (add-to-list 'name-and-pos (cons name position))))))))
      (addsymbols imenu--index-alist))
    (let* ((selected-symbol (ido-completing-read "Symbol? " symbol-names))
           (position (cdr (assoc selected-symbol name-and-pos))))
      (goto-char position))))


(defun coding-hook ()
  "Enable things that are convenient across all coding buffers."
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (make-local-variable 'column-number-mode)
  (column-number-mode t)
  (setq save-place t)
  (auto-fill-mode) ;; in comments only
  (if window-system (hl-line-mode t))
  (pretty-lambdas))


(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))


;; Cosmetic
(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))


(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))


(defun kill-current-line ()
  "Kill the current line, no matter where the cursor is."
  (interactive)
  (textmate-select-line) (kill-region (region-beginning) (region-end)))
(global-set-key [(control shift k)] 'kill-current-line)


(defun sudo-edit (&optional arg)
  (interactive "p")
  (if (or arg (not buffer-file-name))
      (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))


(provide 'customFunctions)
