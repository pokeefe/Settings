;;
;; Flymake
;;

;; need to do this now so that the following customizations have a chance to override
;; what's in flymake.el instead of missing the opportunity because of autoloading.
(require 'flymake)

;; when over a flymake error, relevant information is displayed in the mini-buffer
(require 'flymake-cursor)


;;
;; LaTeX Specific
;;


;; LaTeX: Enable flymake for texlive distribution of LaTeX
(defun flymake-get-tex-args (file-name)
  (list "pdflatex" (list "-shell-escape"
                         "-file-line-error"
                         "-draftmode"
                         "-interaction=nonstopmode"
                         "-output-directory"
                         temporary-file-directory
                         file-name)))


(defun flymake-create-temp-intemp (file-name prefix)
  "Return file name in temporary directory for checking FILE-NAME.
This is a replacement for `flymake-create-temp-inplace'. The
difference is that it gives a file name in
`temporary-file-directory' instead of the same directory as
FILE-NAME.

For the use of PREFIX see that function.

Note that not making the temporary file in another directory
\(like here) will not if the file you are checking depends on
relative paths to other files \(for the type of checks flymake
makes)."
  (unless (stringp file-name)
    (error "Invalid file-name"))
  (or prefix
      (setq prefix "flymake"))
  (let* ((name (concat
                (file-name-nondirectory
                 (file-name-sans-extension file-name))
                "_" prefix))
         (ext  (concat "." (file-name-extension file-name)))
         (temp-name (concat temporary-file-directory (concat name ext))))
    (flymake-log 3 "create-temp-intemp: file=%s temp=%s" file-name temp-name)
    temp-name))


(defun flymake-simple-tex-init ()
  (flymake-get-tex-args (flymake-init-create-temp-buffer-copy 'flymake-create-temp-intemp)))

(defun flymake-master-tex-init ()
  (let* ((temp-master-file-name (flymake-init-create-temp-source-and-master-buffer-copy
                                 'flymake-get-include-dirs-dot 'flymake-create-temp-intemp
                                 '("\\.tex\\'")
                                 "[ \t]*\\input[ \t]*{\\(.*%s\\)}")))
    (when temp-master-file-name
      (flymake-get-tex-args temp-master-file-name))))


(provide 'init-flymake)
