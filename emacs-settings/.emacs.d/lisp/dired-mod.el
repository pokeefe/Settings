;; Make dired only search for filenames, not the entire buffer text
(setq dired-isearch-filenames t)
(setq wdired-allow-to-change-permissions t)
(setq find-name-arg "-iname")

(setq dired-listing-switches "-alh")

(setq dired-recursive-deletes 'always)
(setq find-ls-option '("-exec ls -lh {} \\;" . "-lh"))

(eval-after-load "dired"
  ;; ok to execute on '.' or '..'
  '(defun dired-get-filename (&optional localp no-error-if-not-filep)
     "In Dired, return name of file mentioned on this line.
        Value returned normally includes the directory name.
        Optional arg LOCALP with value `no-dir' means don't include directory
        name in result.  A value of `verbatim' means to return the name exactly as
        it occurs in the buffer, and a value of t means construct name relative to
        `default-directory', which still may contain slashes if in a subdirectory.
        Optional arg NO-ERROR-IF-NOT-FILEP means treat `.' and `..' as
        regular filenames and return nil if no filename on this line.
        Otherwise, an error occurs in these cases."
     (let (case-fold-search file p1 p2 already-absolute)
       (save-excursion
         (if (setq p1 (dired-move-to-filename (not no-error-if-not-filep)))
             (setq p2 (dired-move-to-end-of-filename no-error-if-not-filep))))
       ;; nil if no file on this line, but no-error-if-not-filep is t:
       (if (setq file (and p1 p2 (buffer-substring p1 p2)))
           (progn
             ;; Get rid of the mouse-face property that file names have.
             (set-text-properties 0 (length file) nil file)
             ;; Unquote names quoted by ls or by dired-insert-directory.
             ;; This code was written using `read' to unquote, because
             ;; it's faster than substituting \007 (4 chars) -> ^G (1
             ;; char) etc. in a lisp loop.  Unfortunately, this decision
             ;; has necessitated hacks such as dealing with filenames
             ;; with quotation marks in their names.
             (while (string-match "\\(?:[^\\]\\|\\`\\)\\(\"\\)" file)
               (setq file (replace-match "\\\"" nil t file 1)))
             (setq file (read (concat "\"" file "\"")))
             ;; The above `read' will return a unibyte string if FILE
             ;; contains eight-bit-control/graphic characters.
             (if (and enable-multibyte-characters
                      (not (multibyte-string-p file)))
                 (setq file (string-to-multibyte file)))))
       (and file (file-name-absolute-p file)
            ;; A relative file name can start with ~.
            ;; Don't treat it as absolute in this context.
            (not (eq (aref file 0) ?~))
            (setq already-absolute t))
       (cond
        ((null file)
         nil)
        ((eq localp 'verbatim)
         file)
        ((and (eq localp 'no-dir) already-absolute)
         (file-name-nondirectory file))
        (already-absolute
         (let ((handler (find-file-name-handler file nil)))
           ;; check for safe-magic property so that we won't
           ;; put /: for names that don't really need them.
           ;; For instance, .gz files when auto-compression-mode is on.
           (if (and handler (not (get handler 'safe-magic)))
               (concat "/:" file)
             file)))
        ((eq localp 'no-dir)
         file)
        ((equal (dired-current-directory) "/")
         (setq file (concat (dired-current-directory localp) file))
         (let ((handler (find-file-name-handler file nil)))
           ;; check for safe-magic property so that we won't
           ;; put /: for names that don't really need them.
           ;; For instance, .gz files when auto-compression-mode is on.
           (if (and handler (not (get handler 'safe-magic)))
               (concat "/:" file)
             file)))
        (t
         (concat (dired-current-directory localp) file))))))

(defun dired-ediff-marked-files ()
  "Run ediff on marked ediff files."
  (interactive)
  (let ((marked-files (dired-get-marked-files)))
    (when (= (safe-length marked-files) 1)
      (ediff-revision (nth 0 marked-files)))

    (when (= (safe-length marked-files) 2)
      (ediff-files (nth 0 marked-files) (nth 1 marked-files)))
    
    (when (= (safe-length marked-files) 3)
      (ediff3 (buffer-file-name (nth 0 marked-files))
              (buffer-file-name (nth 1 marked-files)) 
              (buffer-file-name (nth 2 marked-files))))))

(defun dired-vc-status-dir ()
  "Run vc status on marked ediff dir."
  (interactive)
  (let* ((marked-files (dired-get-marked-files))
         (dir (nth 0 marked-files)))
    (vc-dir dir)))

(defun dired-get-size ()
  (interactive)
  (let ((files (dired-get-marked-files)))
    (with-temp-buffer
      (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
      (message "Size of all marked files: %s"
               (progn 
                 (re-search-backward "\\(^[0-9.,]+[A-Za-z]+\\).*total$")
                 (match-string 1))))))

(defun dired-view-file-other-window ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
      (view-file-other-window file)))

(defun dired-find-file-other-window ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
      (find-file-other-window file)))

(defun dired-find-file-mod ()
  (interactive)
  (let ((file (dired-get-file-for-visit)))
    (if (car (file-attributes file))
        (dired-find-file)
      (dired-find-file-other-window))))

(defun dired-kill-and-next-subdir ()
  (interactive)
  (let* ((subdir-name (dired-current-directory))
         (parent-dir  (file-name-directory (directory-file-name subdir-name)))
         (search-term (concat " " (file-basename subdir-name))))
    (dired-kill-subdir)
    (dired-goto-subdir parent-dir)
    (search-forward search-term)))

(defun dired-cd ()
  (interactive)
  (let ((dir (dired-get-file-for-visit)))
    ;; check if directory
    (if (car (file-attributes dir))
        (cd dir))))

(defun dired-cd-home ()
  (interactive)
  (cd (getenv "HOME")))

(defun dired-run-shell ()
  (interactive)
  (term "/bin/bash"))

(add-hook 'dired-mode-hook (lambda ()
                             (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)
                             (define-key dired-mode-map "/" 'dired-isearch-filenames)
                             (define-key dired-mode-map "v" 'dired-view-file-other-window)
                             (define-key dired-mode-map "f" 'dired-find-file-other-window)
                             (define-key dired-mode-map "F" 'find-name-dired)
                             (define-key dired-mode-map "c" 'dired-run-shell)
                             (define-key dired-mode-map "w" 'dired-cd)
                             (define-key dired-mode-map "W" 'dired-cd-home)
                             (define-key dired-mode-map "o" 'dired-do-async-shell-command)
                             (define-key dired-mode-map "k" 'dired-kill-and-next-subdir)
                             (define-key dired-mode-map "K" 'dired-kill-subdir)
                             (define-key dired-mode-map "=" 'dired-ediff-marked-files)
                             (define-key dired-mode-map ";" 'dired-vc-status-dir)
                             (define-key dired-mode-map (kbd "?") 'dired-get-size)
                             (define-key dired-mode-map (kbd "RET") 'dired-find-file-mod)
                             (define-key dired-mode-map (kbd "C-o") 'other-window)
                             (define-key dired-mode-map (kbd "M-p") 'dired-up-directory)))

(provide 'dired-mod)