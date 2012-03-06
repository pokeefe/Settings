;;
;; Emacs Lisp
;;

(define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)
(define-key emacs-lisp-mode-map (kbd "M-.") 'find-function-at-point)


(defun remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
            (lambda ()
              (if (file-exists-p (concat buffer-file-name "c"))
                  (delete-file (concat buffer-file-name "c"))))))

(defun init-emacs-lisp-mode-defaults ()
  (turn-on-eldoc-mode)
  (remove-elc-on-save))


(add-hook 'emacs-lisp-mode-hook (lambda () (init-emacs-lisp-mode-defaults)))


(provide 'init-lisp)
