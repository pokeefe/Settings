(if (eq system-type 'darwin)
    (let ((path))
    (setq path (concat "/opt/local/bin:"
                       "/opt/local/sbin:"
                       "/usr/bin:"
                       "/bin:"
                       "/usr/sbin:"
                       "/sbin:"
                       "/usr/local/bin:"
                       "/usr/texbin:"
                       "/usr/X11/bin:"))
                        (setenv "PATH" path)))


(add-to-list 'load-path "~/.emacs.d/")
(load-file "~/.emacs.d/init.el")
