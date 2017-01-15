(autoload 'org "org" "Org mode." t)

(setq org-agenda-files (quote ("~/org")))

(setq org-startup-indented t)             ;; indent tasks and only show one star
(setq org-log-done nil)                   ;; no timestamp when task moves to DONE
(setq org-enforce-todo-dependencies t)    ;; can't finish a task when a subtask is incomplete
;; (setq org-agenda-todo-list-sublevels nil) ;; only show top-level TODO tasks in agenda todo list
(setq org-clock-clocktable-default-properties '(:maxlevel 4))
(setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)) ;; format time to not show days in clock tables

(setq org-todo-keywords
      '((type "TODO" "TRACKER" "REFILE" "|" "LATER" "WONTFIX" "DONE")))

(add-hook 'org-mode-hook (lambda ()

                           (defun nc-org-new-tracker ()
                             (interactive)
                             (end-of-buffer)
                             (insert (concat "\n*** " "asdf" "\n:PROPERTIES:\n:pivotal:\n:END:\n" )))


                           (defun org-small-agenda-list (todo-keyword)
                             (interactive)
                             (org-todo-list todo-keyword)
                             (shrink-window-if-larger-than-buffer)
                             (other-window 1))

                           (defun org-date-change (new-time)
                             (interactive "snew time: ")
                             (search-backward " ")
                             (forward-char)
                             (zap-up-to-char 1 (string-to-char "]"))
                             (insert new-time))

                           (defun org-date-change-minutes ()
                             (interactive "")
                             (search-forward "]")
                             (backward-char)
                             (backward-char))

                           (defun org-date-change-hours ()
                             (interactive "")
                             (search-forward "]")
                             (backward-char)
                             (backward-char)
                             (backward-char)
                             (backward-char)
                             (backward-char))

                           (defun org-insert-property (prop)
                             (interactive "sproperty: ")
                             (org-beginning-of-line)
                             (insert (concat ":" (upcase prop) ": ")))

                           (defun org-journal-entry ()
                             (interactive)
                             (end-of-buffer)
                             (insert "\n\n")
                             (delete-blank-lines)
                             (insert "\n** ")

                             (setq current-prefix-arg '(16))      ; C-u C-u
                             (call-interactively 'org-time-stamp)
                             (insert "\n"))

                           ;; (defun org-update-journal-entry ()
                           ;;   (interactive)
                           ;;   (org-journal-entry)
                           ;;   (end-of-buffer)
                           ;;   (outline-previous-visible-heading 1)
                           ;;   (org-metaup 1))

                           ;; (linum-mode 0) ;; disable line numbers
                           ;; (global-linum-mode 0) ;; disable line numbers
                           (visual-line-mode)    ;; wrap long lines

                           ;; ;; a == agenda
                           ;; ;; (just a todo list, not what org mode considers an "agenda")
                           ;; (global-set-key (kbd "C-c o a a") (lambda () (interactive) (org-small-agenda-list ""))) ; all
                           ;; (global-set-key (kbd "C-c o a t") (lambda () (interactive) (org-small-agenda-list "TODO")))
                           ;; (global-set-key (kbd "C-c o a d") (lambda () (interactive) (org-small-agenda-list "DONE")))


                           ;; ;; j e == journal entry
                           (global-set-key (kbd "C-c o j e") 'org-journal-entry)
                           ;; (global-set-key (kbd "C-c o j u") 'org-update-journal-entry)

                           (global-set-key (kbd "C-c i c") (lambda () (interactive) (beginning-of-line) (org-kill-line) (insert "- [ ] ")))

                           (global-set-key (kbd "C-c c i") 'org-clock-in)
                           (global-set-key (kbd "C-c c o") 'org-clock-out)
                           (global-set-key (kbd "C-c c g") 'org-clock-goto)

                           ;; (smartrep-define-key
                           ;;     my-keys-minor-mode-map "C-c" '(("t" . 'org-todo)
                           ;;                                    ("C-n" . 'outline-next-visible-heading)
                           ;;                                    ("C-p" . 'outline-previous-visible-heading)
                           ;;                                    ))


                           ;; l == link
                           (global-set-key (kbd "C-c o l c") 'org-store-link) ; copy
                           (global-set-key (kbd "C-c o l p") 'org-insert-link) ; paste
                           (global-set-key (kbd "C-c o l f") 'org-open-at-point) ; follow

                           (defun beginning-of-org-line ()
                             (interactive)
                             (beginning-of-line)
                             (search-forward " ")
                             )

                           ;; FIXME this isn't overriding the existing mapping (probably because I'm
                           ;; trying to set it on the global map when I should use an org-specific
                           ;; map)
                           ;; beginning-of-org-line works as intended though :)
                           (global-set-key (kbd "M-a") 'beginning-of-org-line)

                           (defun insert-pivotal-subtree (id-or-url)
                             "for running this pivotal script"
                             (interactive "surl or id: ")
                             (insert (shell-command-to-string (concat "./piv.sh " (replace-regexp-in-string "#" "\#" id-or-url))))
                             (search-backward ":PROPERTIES")
                             (previous-line)
                             (beginning-of-line)
                             (forward-char)
                             (forward-char)
                             (forward-char)
                             (forward-char)
                             )


                           ;; (global-set-key (kbd "C-c o p") 'insert-pivotal-subtree)
                           ;; (global-set-key (kbd "C-c C-o p") 'insert-pivotal-subtree)

                           ;; (global-set-key (kbd "C-c C-o l") 'org-todo-list)

                           (defun all-the-way-up ()
                             (interactive)
                             (org-metaup)
                             (org-metaup)
                             (org-metaup)
                             (org-metaup)
                             (org-metaup)
                             (org-metaup)
                             (org-metaup)
                             (org-metaup)
                             (org-metaup)
                             (org-metaup)
                             (org-metaup)
                             (org-metaup)
                             (org-metaup)
                             (org-metaup)
                             )

                           (defun all-the-way-down ()
                             (interactive)
                             (org-metadown)
                             (org-metadown)
                             (org-metadown)
                             (org-metadown)
                             (org-metadown)
                             (org-metadown)
                             (org-metadown)
                             (org-metadown)
                             (org-metadown)
                             (org-metadown)
                             (org-metadown)
                             (org-metadown)
                             (org-metadown)
                             (org-metadown)
                             )

                           (global-set-key (kbd "M-n") 'org-metadown)
                           (global-set-key (kbd "M-p") 'org-metaup)
                           (global-set-key (kbd "ESC M-n") 'all-the-way-down)
                           (global-set-key (kbd "ESC M-p") 'all-the-way-up)

                           ;; m s == move subtree
                           (smartrep-define-key
                               my-keys-minor-mode-map "C-c o s" '(("b" . 'org-promote-subtree)
                                                                  ("f" . 'org-demote-subtree)
                                                                  ("p" . 'org-metaup)
                                                                  ("n" . 'org-metadown)
                                                                  ("t" . (lambda () (interactive) (org-todo "TODO")))
                                                                  ("d" . (lambda () (interactive) (org-todo "DONE")))
                                                                  ("l" . (lambda () (interactive) (org-todo "LATER")))
                                                                  ("w" . (lambda () (interactive) (org-todo "WONTFIX")))
                                                                  ("C-n" . 'next-line)
                                                                  ("C-p" . 'previous-line)
                                                                  ("C-v" . 'org-cut-subtree)
                                                                  ("M-v" . 'org-copy-subtree)
                                                                  ("C-y" . 'org-paste-subtree)
                                                                  ("a" . 'org-archive-subtree)
                                                                  ))

                           ;; n == navigate
                           (smartrep-define-key
                               my-keys-minor-mode-map "C-c o n" '(("b" . 'org-backward-heading-same-level)
                                                                  ("f" . 'org-forward-heading-same-level)
                                                                  ("u" . 'outline-up-heading)
                                                                  ("p" . 'outline-previous-visible-heading)
                                                                  ("n" . 'outline-next-visible-heading)
                                                                  ("C-b" . 'backward-char)
                                                                  ("C-f" . 'forward-char)
                                                                  ("C-n" . 'next-line)
                                                                  ("C-p" . 'previous-line)))

                           ;; d == date
                           (smartrep-define-key
                               my-keys-minor-mode-map "C-c o d" '(("b" . 'org-timestamp-down-day)
                                                                  ("f" . 'org-timestamp-up-day)
                                                                  ("p" . 'org-timestamp-up)
                                                                  ("n" . 'org-timestamp-down)
                                                                  ("m" . 'org-date-change-minutes)
                                                                  ("h" . 'org-date-change-hours)
                                                                  ("C-f" . 'forward-char)
                                                                  ("C-b" . 'backward-char)))

                           (add-hook 'before-save-hook 'org-align-all-tags)
                           ;; not sure if this is working...
                           ;; (add-hook 'before-save-hook (lambda () (org-update-statistics-cookies t)))
                           ))
