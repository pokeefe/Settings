;;
;; For prog-mode derived modes
;;

(require 'imenu)

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
          (ido-enable-flex-matching
           (if (boundp 'ido-enable-flex-matching)
               ido-enable-flex-matching t))
          name-and-pos symbol-names position)
      (unless ido-mode
        (ido-mode 1)
        (setq ido-enable-flex-matching t))
      (while (progn
               (imenu--cleanup)
               (setq imenu--index-alist nil)
               (ido-goto-symbol (imenu--make-index-alist))
               (setq selected-symbol
                     (ido-completing-read "Symbol? " symbol-names))
               (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
        (push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
        (goto-char (overlay-start position)))
       (t
        (goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
        (cond
         ((and (listp symbol) (imenu--subalist-p symbol))
          (ido-goto-symbol symbol))
         ((listp symbol)
          (setq name (car symbol))
          (setq position (cdr symbol)))
         ((stringp symbol)
          (setq name symbol)
          (setq position
                (get-text-property 1 'org-imenu-marker symbol))))
        (unless (or (null position) (null name)
                    (string= (car imenu--rescan-item) name))
          (add-to-list 'symbol-names name)
          (add-to-list 'name-and-pos (cons name position))))))))


;; show the name of the current function definition in the modeline
(require 'which-func)
(which-func-mode 1)


;; ;; We have a number of turn-on-* functions since it's advised that lambda
;; ;; functions not go in hooks. Repeatedly evaluating an add-to-list with a
;; ;; hook value will repeatedly add it since there's no way to ensure
;; ;; that a lambda doesn't already exist in the list.

(defun turn-on-local-comment-auto-fill ()
  (set (make-local-variable 'comment-auto-fill-only-comments) t)
  (auto-fill-mode t))


(defun turn-on-local-column-number-mode ()
  (make-local-variable 'column-number-mode)
  (column-number-mode t))


(defun turn-on-hl-line-mode ()
  (when (> (display-color-cells) 8)
    (hl-line-mode t)))


(defun turn-on-whitespace-mode ()
  (whitespace-mode t))


(defun turn-on-idle-highlight-mode ()
  (idle-highlight-mode t))

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(?\\(lambda\\>\\)"
          (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                    ,(make-char 'greek-iso8859-7 107))
                    nil))))))

(defun add-watchwords ()
  (font-lock-add-keywords
   nil '(("\\<\\(FIX\\|TODO\\|FIXME\\|HACK\\|REFACTOR\\):"
          1 font-lock-warning-face t))))



(defun prog-mode-defaults ()
  "Default coding hook, useful with any programming language."
  (flyspell-prog-mode)
  (turn-on-local-comment-auto-fill)
  (turn-on-whitespace-mode)
  (turn-on-hl-line-mode)
  (turn-on-whitespace-mode)
  (turn-on-idle-highlight-mode)
  (pretty-lambdas)
  (add-watchwords)
  ;; keep the whitespace decent all the time (in this buffer)
  (add-hook 'before-save-hook 'whitespace-cleanup nil t))


(add-hook 'prog-mode-hook (lambda () (prog-mode-defaults)))

(provide 'init-programming)
