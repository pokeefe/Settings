;;
;; LaTeX
;;

(setq LaTeX-command "latex -shell-escape")

(load "auctex.el" nil t t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)

(load "preview-latex.el" nil t t)
(setq preview-auto-cache-preamble nil)

;; need to do this now so that the tex-args are applied.
(require 'flymake)

;; LaTeX: Enable flymake for texlive distribution of LaTeX
(defun flymake-get-tex-args (file-name)
  (list "/usr/texbin/pdflatex" (list "-shell-escape" "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))


;; apply LaTeX hooks (spellcheck, etc.)
(add-hook 'LaTeX-mode-hook (lambda ()
                             (flyspell-mode)
                             (outline-minor-mode)
                             (turn-on-auto-fill)
                             (setq latex-mode-map LaTeX-mode-map)
                             (yas/reload-all)
                             (orgtbl-mode)
                             (flymake-mode)))


(defun orgtbl-to-latex-matrix (table params)
  "Convert the Orgtbl mode TABLE to a LaTeX Matrix."
  (interactive)
  (let* ((params2
          (list
           :tstart (concat "\\[\n\\begin{bmatrix}")
           :tend "\\end{bmatrix}\n\\]"
           :lstart "" :lend " \\\\" :sep " & "
           :efmt "%s\\,(%s)" :hline "\\hline")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))


(defun orgtbl-insert-matrix ()
  "Insert a radio table template appropriate for this major mode."
  (interactive)
  (let* ((txt orgtbl-latex-matrix-string)
         name pos)
    (setq name (read-string "Table name: "))
    (while (string-match "%n" txt)
      (setq txt (replace-match name t t txt)))
    (or (bolp) (insert "\n"))
    (setq pos (point))
    (insert txt)
    (previous-line)
    (previous-line)))

(defcustom orgtbl-latex-matrix-string  "% BEGIN RECEIVE ORGTBL %n
% END RECEIVE ORGTBL %n
\\begin{comment}
#+ORGTBL: SEND %n orgtbl-to-latex-matrix :splice nil :skip 0

\\end{comment}\n"
  "Template for the latex matrix orgtbl translator
All occurrences of %n in a template will be replaced with the name of the
table, obtained by prompting the user."
  :type 'string
  :group 'org-table)




;; Autopair functionality in LaTeX mode
(add-hook 'LaTeX-mode-hook
          #'(lambda ()
              (set (make-local-variable 'autopair-handle-action-fns)
                   (list #'autopair-default-handle-action
                         #'autopair-latex-mode-paired-delimiter-action))))

;; Add $ to the autopair list for Latex-mode
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

(provide 'init-latex)
