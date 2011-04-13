(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-view-program-list (quote (("open" ""))))
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Evince") (output-html "xdg-open"))))
 '(blink-cursor-mode nil)
 '(cc-other-file-alist (quote (("\\.cc\\'" (".hh" ".h")) ("\\.hh\\'" (".cc" ".C")) ("\\.m\\'" (".h")) ("\\.c\\'" (".h")) ("\\.h\\'" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m")) ("\\.C\\'" (".H" ".hh" ".h")) ("\\.H\\'" (".C" ".CC")) ("\\.CC\\'" (".HH" ".H" ".hh" ".h")) ("\\.HH\\'" (".CC")) ("\\.c\\+\\+\\'" (".h++" ".hh" ".h")) ("\\.h\\+\\+\\'" (".c++")) ("\\.cpp\\'" (".hpp" ".hh" ".h")) ("\\.hpp\\'" (".cpp")) ("\\.cxx\\'" (".hxx" ".hh" ".h")) ("\\.hxx\\'" (".cxx")))))
 '(flyspell-issue-welcome-flag nil)
 '(fringe-mode 0 nil (fringe))
 '(ido-ignore-directories (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`auto/" "\\\\.prv/")))
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "_region_" "\\\\.prv/" "\\`auto/" "\\.DS_Store" "_flymake\\." "\\.pyg" "\\.synctex\\.gz")))
 '(javascript-indent-level 2)
 '(org-agenda-files (quote ("~/Dropbox/Org/gtd.org" "~/Dropbox/Org/inbox.org")))
 '(org-capture-templates (quote (("t" "Todo" entry (file+headline "~/Dropbox/Org/inbox.org" "Inbox") "* TODO %?
    Added: %U") ("a" "Appointment" entry (file+headline "~/Dropbox/Org/gtd.org" "Calendar") "* APPT %? %^T
    Added %U") ("p" "Note" entry (file+headline "~/Dropbox/Org/notes.org" "") "
* %^{topic} %T 
%i%?") ("f" "Todo with current file link" entry (file+headline "~/Dropbox/Org/inbox.org" "Inbox") "* TODO %?
    Added: %U
    File: %A") ("s" "Someday" entry (file+headline "~/Dropbox/Org/someday.org" "Someday") "* TODO %?
    Added: %U"))))
 '(org-hide-leading-stars t)
 '(org-odd-levels-only t)
 '(org-refile-targets (quote (("~/Dropbox/Org/gtd.org" :maxlevel . 1) ("~/Dropbox/Org/someday.org" :maxlevel . 2))))
 '(org-startup-folded (quote content))
 '(preview-gs-options (quote ("-q" "-dSAFER" "-dNOPAUSE" "-DNOPLATFONTS" "-dPrinted" "-dTextAlphaBits=4" "-dGraphicsAlphaBits=4")))
 '(preview-image-type (quote png))
 '(preview-transparent-color t)
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(org-hide ((((background dark)) (:foreground "#092E52"))))
 '(paren-face ((((class color) (background dark)) (:foreground "white")))))
