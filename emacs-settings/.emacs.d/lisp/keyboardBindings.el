

;; Some Mac-friendly key counterparts
(global-set-key [(meta s)] 'save-buffer)
(global-set-key [(meta z)] 'undo)
(global-set-key [(meta c)] 'kill-ring-save)
(global-set-key [(meta shift v)] 'clipboard-yank)

;; Keyboard Overrides
(define-key text-mode-map [(meta s)] 'save-buffer)
(global-set-key [(control o)] 'other-window)
(global-set-key [(meta N)] 'cleanup-buffer)

;; Align your code in a pretty way.
(global-set-key [(control x)(\\)] 'align-regexp)

(global-set-key [(meta /)] 'comment-or-uncomment-region-or-line)

;; Use regex searches by default.
(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)
(global-set-key [(control meta s)] 'isearch-forward)
(global-set-key [(control meta r)] 'isearch-backward)
(global-set-key [(meta %)] 'query-replace-regexp)
(global-set-key [(control meta %)] 'query-replace)


;; Jump to a definition in the current file. (This is awesome.)
(global-set-key [(control x)(control i)] 'ido-imenu)

;; File finding
(global-set-key [(control x)(meta f)] 'ido-find-file-other-window)
(global-set-key [(control x)(control meta f)] 'find-file-in-project)
;; (global-set-key [(control x)(f)] 'recentf-ido-find-file)
(global-set-key [(control x)(control b)] 'ibuffer)

;; Start a regular shell
(global-set-key [(control x)(control m)] 'shell)

;; Help should search more than just commands
(global-set-key [(control h)(a)] 'apropos)

;; Should be able to eval-and-replace anywhere.
(global-set-key [(control c)(e)] 'eval-and-replace)

;; Find file counterparts (really useful)
(global-set-key [(control meta up)] 'ff-find-other-file)

(provide 'keyboardBindings)
