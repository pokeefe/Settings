;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Matlab Initialization Stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/vendor/matlab-emacs")
(autoload 'matlab-mode "matlab" "Enter MATLAB mode." t)
(setq auto-mode-alist (cons '("\\.m\\'" . matlab-mode) auto-mode-alist))

(provide 'matlabInit)
