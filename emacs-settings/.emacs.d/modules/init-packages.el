;;
;; Packages
;;

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar my-packages '(smex
                      ido-ubiquitous
                      idle-highlight-mode
                      expand-region
                      ack-and-a-half
                      auctex
                      deft
                      expand-region
                      groovy-mode
                      markdown-mode
                      python
                      solarized-theme
                      volatile-highlights
                      zenburn-theme
                      color-theme
                      cmake-mode
                      yasnippet)
  "A list of packages that will be installed at launch (if missing).")

(defun init-packages-installed-p ()
  (loop for p in my-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(unless (init-packages-installed-p)
  ;; check for new packages (package versions)
  (message "%s" "Emacs is now refreshing its package database...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; install the missing packages
  (dolist (p my-packages)
    (when (not (package-installed-p p))
      (package-install p))))

(provide 'init-packages)

