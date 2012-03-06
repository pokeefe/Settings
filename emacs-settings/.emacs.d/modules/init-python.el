;;
;; Python
;;

(require 'python)

(defun init-python-mode-defaults ()
  (run-hooks 'init-prog-mode-hook) ;; run manually; not derived from prog-mode
  (electric-indent-mode -1))

(add-hook 'python-mode-hook (lambda () (init-python-mode-defaults)))

(provide 'init-python)

