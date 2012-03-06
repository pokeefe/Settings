;; Cobalt Color Theme for Emacs.
;;
;; Based loosely off color-theme-twilight by Marcus Crafter and default themes in the color-theme package
;; I'm sure I am missing some stuff, feel free to fork and fix
;;
;; To use add the following to your .emacs file
;; (require 'color-theme)
;; (color-theme-initialize)
;; (load-file "~/.emacs.d/color-theme-cobalt/color-theme-cobalt.el")
;; (color-theme-cobalt)
;; 
;; Thanks to Jacob Rus, the Textmate Cobalt author and Marcus Crafter
;;
(defun color-theme-cobalt ()
  "Color based off the TextMate Cobalt theme by Jacob Rus 
   and the Textmate Twilight Color Theme port by Marcus Crafter"
  (interactive)
  (color-theme-install
	'(color-theme-cobalt
          ((background-color . "#092E52")
           (foreground-color . "white"))
          nil
          (default ((t (nil))))
          ; (fringe ((t (:background "lightgray"))))
		  (fringe ((t (nil))))
	  (default ((t (:background "#141414" :foreground "#CACACA"))))
	  (blue ((t (:foreground "blue"))))
	  (border-glyph ((t (nil))))
	  (buffers-tab ((t (:background "#141414" :foreground "#CACACA"))))
	  (font-lock-builtin-face ((t (:foreground "#CACACA"))))
	  (font-lock-comment-face ((t (:italic t :foreground "#329BFF"))))
	  (font-lock-constant-face ((t (:foreground "#FB7A9E"))))
	  (font-lock-doc-string-face ((t (:foreground "DarkOrange"))))
	  (font-lock-function-name-face ((t (:foreground "#FCE200"))))
	  (font-lock-keyword-face ((t (:foreground "#FBAD00"))))
	  (font-lock-preprocessor-face ((t (:foreground "Aquamarine"))))
	  (font-lock-reference-face ((t (:foreground "SlateBlue"))))

	  (font-lock-regexp-grouping-backslash ((t (:foreground "#E9C062"))))
	  (font-lock-regexp-grouping-construct ((t (:foreground "red"))))

	  (minibuffer-prompt ((t (:foreground "#FCFD00"))))
	  (ido-subdir ((t (:foreground "#CF6A4C"))))
	  (ido-first-match ((t (:weight bold :foreground "#FCFD00"))))
	  (ido-only-match ((t (:weight bold :foreground "#8F9D6A"))))
	  (mumamo-background-chunk-submode ((t (:background "#222222")))) 
	  (linum ((t (:foreground "grey50" :background "lightgray"))))

	  (font-lock-string-face ((t (:foreground "#53DD00"))))
	  (font-lock-type-face ((t (:foreground "#FCFD00"))))
	  (font-lock-warning-face ((t (:background "#EE799F" :foreground "red"))))
	  (gui-element ((t (:background "#D4D0C8" :foreground "black"))))
	  (region ((t (:background "#27292A"))))
          (modeline ((t (:background "lightgray" :foreground "black" :box (:line-width -1 :style released-button :family "helv")))))
          (modeline-buffer-id ((t (:background "lightgray" :foreground "#092E52" :slant normal :weight bold :width normal :family "outline-verdana"))))
          (modeline-mousable ((t (:background "white" :foreground "DeepSkyBlue3"))))
          (modeline-mousable-minor-mode ((t (:background "white" :foreground "DeepSkyBlue3"))))
	  (highlight ((t (:background "#111111"))))
	  (highline-face ((t (:background "SeaGreen"))))
	  (left-margin ((t (nil))))
	  (text-cursor ((t (:background "yellow" :foreground "black"))))
	  (toolbar ((t (nil))))
	  (underline ((nil (:underline nil))))
          (ecb-token-header-face ((t (:background "#092E52"))))
          (font-lock-variable-name-face ((t (:italic t :bold t :foreground "LightGoldenrod" :slant italic :weight bold))))

     ;; (org-hide ((t (:foreground "#2e3436")))) 
     ;; (org-level-1 ((t (:bold t :foreground "dodger blue" :height 1.5))))
     ;; (org-level-2 ((t (:bold nil :foreground "#edd400" :height 1.2))))
     ;; (org-level-3 ((t (:bold t :foreground "#6ac214" :height 1.0))))
     ;; (org-level-4 ((t (:bold nil :foreground "tomato" :height 1.0))))
     ;; (org-date ((t (:underline t :foreground "magenta3"))))
     ;; (org-footnote  ((t (:underline t :foreground "magenta3"))))
     ;; (org-link ((t (:foreground "skyblue2" :background "#2e3436"))))
     ;; (org-special-keyword ((t (:foreground "brown"))))
     ;; (org-verbatim ((t (:foreground "#eeeeec" :underline t :slant italic))))
     ;; (org-block ((t (:foreground "#bbbbbc"))))
     ;; (org-quote ((t (:inherit org-block :slant italic))))
     ;; (org-verse ((t (:inherit org-block :slant italic))))
     ;; (org-todo ((t (:bold t :foreground "Red"))))
     ;; (org-done ((t (:bold t :foreground "ForestGreen"))))
     ;; (org-agenda-structure ((t (:weight bold :foreground "tomato"))))
     ;; (org-agenda-date ((t (:foreground "#6ac214"))))
     ;; (org-agenda-date-weekend ((t (:weight normal :foreground "dodger blue"))))
     ;; (org-agenda-date-today ((t (:weight bold :foreground "#edd400"))))
	  (zmacs-region ((t (:background "snow" :foreground "blue")))))))
