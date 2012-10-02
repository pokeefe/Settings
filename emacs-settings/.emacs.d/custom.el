(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-PDF-mode t)
 '(TeX-view-program-list (quote (("open" ""))) t)
 '(TeX-view-program-selection (quote (((output-dvi style-pstricks) "dvips and gv") (output-dvi "xdvi") (output-pdf "Evince") (output-html "xdg-open"))) t)
 '(c-basic-offset 4 t)
 '(cc-other-file-alist (quote (("\\.cc\\'" (".hh" ".h")) ("\\.hh\\'" (".cc" ".C")) ("\\.m\\'" (".h")) ("\\.c\\'" (".h")) ("\\.h\\'" (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m")) ("\\.C\\'" (".H" ".hh" ".h")) ("\\.H\\'" (".C" ".CC")) ("\\.CC\\'" (".HH" ".H" ".hh" ".h")) ("\\.HH\\'" (".CC")) ("\\.c\\+\\+\\'" (".h++" ".hh" ".h")) ("\\.h\\+\\+\\'" (".c++")) ("\\.cpp\\'" (".hpp" ".hh" ".h")) ("\\.hpp\\'" (".cpp")) ("\\.cxx\\'" (".hxx" ".hh" ".h")) ("\\.hxx\\'" (".cxx")))))
 '(cua-enable-cua-keys nil)
 '(flyspell-issue-message-flag nil)
 '(flyspell-issue-welcome-flag nil)
 '(fringe-mode 0 nil (fringe))
 '(grep-command "grep -nHi -e ")
 '(grep-find-command (quote ("find . -type f -exec grep -nHi -e  {} +" . 34)))
 '(grep-highlight-matches (quote auto))
 '(ido-enable-tramp-completion nil)
 '(ido-ignore-directories (quote ("\\`CVS/" "\\`\\.\\./" "\\`\\./" "\\`auto/" "\\\\.prv/")))
 '(ido-ignore-files (quote ("\\`CVS/" "\\`#" "\\`.#" "\\`\\.\\./" "\\`\\./" "_region_" "\\\\.prv/" "\\`auto/" "\\.DS_Store" "_flymake\\." "\\.pyg" "\\.synctex\\.gz")))
 '(matlab-auto-fill nil)
 '(matlab-fill-code nil)
 '(matlab-use-eei nil)
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
 '(org-refile-targets (quote (("~/Google Drive/Org/gtd.org" :maxlevel . 2) ("~/Google Drive/Org/occipital.org" :maxlevel . 2))))
 '(org-startup-folded t)
 '(preview-auto-reveal t)
 '(preview-gs-command "/usr/local/bin/gs")
 '(reb-re-syntax (quote string))
 '(show-paren-mode t)
 '(user-mail-address "patokeefe1@gmail.com"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
