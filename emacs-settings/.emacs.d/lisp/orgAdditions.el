;; org-mode
(require 'org-install)

;;(require 'org)
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(setq org-directory "~/Dropbox/Org")

(setq org-default-notes-file (concat org-directory "/notes.org"))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; The first three don't really matter. It's the textmate-mode
;; override that is super important here. It needs to be nil instead
;; of org-mode-map because org-mode has many many maps that are loaded
;; depending on context.
(add-hook 'org-mode-hook
          (lambda()
            (local-set-key [(control shift left)] 'previous-buffer)
            (local-set-key [(control shift right)] 'next-buffer)
            (local-set-key [(meta shift right)] 'ido-switch-buffer)
            (add-to-list 'minor-mode-overriding-map-alist (cons 'textmate-mode nil))
            (hl-line-mode t)
            ))

;; this makes control-tab function like org-mode
(add-hook 'outline-minor-mode-hook
	  (lambda ()
	    (define-key outline-minor-mode-map [(control tab)] 'org-cycle)
        (define-key outline-minor-mode-map [(shift tab)] 'org-global-cycle)))

;; Include diary in agenda
(setq org-agenda-include-diary t)

;; Many GTD related variables
(setq org-agenda-span '7
      org-agenda-restore-windows-after-quit t
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      org-deadline-warning-days 3
      org-fast-tag-selection-single-key nil
      org-reverse-note-order nil
      org-tags-match-list-sublevels nil
      org-time-stamp-rounding-minutes 5
      org-use-fast-todo-selection t
      org-use-tag-inheritance nil)

;;org-agenda-sorting-strategy
;;org-agenda-todo-ignore-***

;; Mobile org specific variables
(setq org-mobile-inbox-for-pull (concat org-directory "/inbox.org"))
(setq org-mobile-directory "~/Dropbox/MobileOrg")

;; Enable clock persistence
(setq org-clock-persist 'history)
(org-clock-persistence-insinuate)


;; Specify todo keyworks
(setq org-todo-keywords '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "APPT(a)" "|" "DONE(d)" "CANCELLED(c)" "DEFERRED(f)")))

;; Log the time when a TODO item was finished
(setq org-log-done 'time)

;; Specify global tags with fast tag selection
(setq org-tag-alist '((:startgroup . nil) ("@office" . ?o) ("@home" . ?h) (:endgroup . nil)
                      ("computer" . ?c) ("reading" . ?r) ("grocery" . ?g) ("homework" . ?w) ("research" . ?r)))

;; Effort and global properties
(setq org-global-properties '(("Effort_ALL". "0 0:10 0:20 0:30 1:00 2:00 3:00 4:00 6:00 8:00")))

;; Set global Column View format
(setq org-columns-default-format '"%38ITEM(Details) %TAGS(Context) %7TODO(To Do) %5Effort(Time){:} %6CLOCKSUM(Clock)")




(setq org-agenda-custom-commands
'(

("P" "Projects"   
((tags "project")))

("H" "Office and Home Lists"
     ((agenda)
          (tags-todo "@office")
          (tags-todo "@home")
          (tags-todo "computer")
          (tags-todo "reading")
          (tags-todo "homework")
          (tags-todo "grocery")))

("D" "Daily Action List"
     (
          (agenda "" ((org-agenda-ndays 1)
                      (org-agenda-sorting-strategy
                       (quote ((agenda time-up priority-down tag-up) )))
                      (org-deadline-warning-days 0)
                      ))))
("I" "Import diary from iCal" agenda ""
         ((org-agenda-mode-hook
           (lambda ()
             (org-mac-iCal)))))
)
)

(defun gtd ()
    (interactive)
    (find-file (concat org-directory "/gtd.org"))
)
(global-set-key (kbd "C-c g") 'gtd)

(defun inbox()
  (interactive)
  (find-file (concat org-directory "/inbox.org"))
)
(global-set-key (kbd "C-c i") 'inbox)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; LaTeX Mac OS X Specific Init
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(when (string-equal system-type "darwin")
  (add-hook 'org-agenda-cleanup-fancy-diary-hook
          (lambda ()
            (goto-char (point-min))
            (save-excursion
              (while (re-search-forward "^[a-z]" nil t)
                (goto-char (match-beginning 0))
                (insert "0:00-24:00 ")))
            (while (re-search-forward "^ [a-z]" nil t)
              (goto-char (match-beginning 0))
              (save-excursion
                (re-search-backward "^[0-9]+:[0-9]+-[0-9]+:[0-9]+ " nil t))
              (insert (match-string 0))))))

(provide 'orgAdditions)

