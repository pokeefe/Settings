;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX Initialization Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq LaTeX-command "latex -shell-escape")

(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(load "preview-latex.el" nil t t)
(setq preview-auto-cache-preamble nil)

;; LaTeX: Enable flymake for texlive distribution of LaTeX
(defun flymake-get-tex-args (file-name)
  (list "/usr/texbin/pdflatex" (list "-shell-escape" "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

(require 'reftex)

;; apply LaTeX hooks (spellcheck, etc.)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (flyspell-mode)
                             (outline-minor-mode)
                             (turn-on-auto-fill)
                             (turn-on-reftex)
                             (setq latex-mode-map LaTeX-mode-map)
                             (yas/reload-all)
                             (flymake-mode)))


;; Autopair functionality in LaTeX mode
(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (set (make-local-variable 'autopair-handle-action-fns)
                   (list #'autopair-default-handle-action
                         #'autopair-latex-mode-paired-delimiter-action))))

;; Add $ to the autopar list for Latex-mode
;; (add-hook 'latex-mode-hook
;;           #'(lambda ()
;;               (push '(?$ . ?$)
;;                    (getf autopair-extra-pairs :everywhere))))


;; assist syncTeX
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(setq TeX-source-correlate-method 'synctex)

;; when over a flymake error, relevant information is displayed in the mini-buffer
(require 'flymake-cursor)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX Mac OS X Specific Init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (string-equal system-type "darwin")
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

  (setq TeX-view-program-selection '((output-pdf "Skim"))))

(provide 'latexInit)
