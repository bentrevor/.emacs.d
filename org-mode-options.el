(require 'org)

(setq org-agenda-files (quote ("~/org")))

(setq org-startup-indented t)             ;; indent tasks and only show one star
(setq org-log-done nil)                   ;; no timestamp when task moves to DONE
(setq org-enforce-todo-dependencies t)    ;; can't finish a task when a subtask is incomplete
(setq org-agenda-todo-list-sublevels nil) ;; only show top-level TODO tasks in agenda todo list
(setq org-clock-idle-time 20)             ;; ask how to clock time after being idle (minutes)

(setq org-todo-keywords
      '((type "TODO(t)" "LATER(l)" "|" "DONE(d)")))

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

(add-hook 'org-mode-hook (lambda ()
                           (linum-mode 0) ;; disable line numbers
                           (global-linum-mode 0) ;; disable line numbers
                           (visual-line-mode)    ;; wrap long lines

                           ;; a == agenda
                           ;; (just a todo list, not what org mode considers an "agenda")
                           (global-set-key (kbd "C-c o a a") (lambda () (interactive) (org-small-agenda-list "")))
                           (global-set-key (kbd "C-c o a t") (lambda () (interactive) (org-small-agenda-list "TODO")))
                           (global-set-key (kbd "C-c o a d") (lambda () (interactive) (org-small-agenda-list "DONE")))

                           ;; t == todo
                           (global-set-key (kbd "C-c o t t") (lambda () (interactive) (org-todo "TODO")))
                           (global-set-key (kbd "C-c o t d") (lambda () (interactive) (org-todo "DONE")))
                           (global-set-key (kbd "C-c o t l") (lambda () (interactive) (org-todo "LATER")))

                           ;; l == link
                           (global-set-key (kbd "C-c o l c") 'org-store-link) ; copy
                           (global-set-key (kbd "C-c o l p") 'org-insert-link) ; paste
                           (global-set-key (kbd "C-c o l f") 'org-open-at-point) ; follow

                           ;; m == move (subtree)
                           (require 'smartrep)
                           (smartrep-define-key
                              global-map "C-c o m" '(("b" . 'org-promote-subtree)
                                                     ("f" . 'org-demote-subtree)
                                                     ("p" . 'org-metaup)
                                                     ("n" . 'org-metadown)))

                           ;; d == date
                           (smartrep-define-key
                               global-map "C-c o d" '(("b" . 'org-timestamp-down-day)
                                                      ("f" . 'org-timestamp-up-day)
                                                      ("p" . 'org-timestamp-up)
                                                      ("n" . 'org-timestamp-down)))

                           (global-set-key (kbd "C-c o d c") 'org-date-change) ; change

                           ;; c == clock
                           (global-set-key (kbd "C-c o c i") 'org-clock-in)
                           (global-set-key (kbd "C-c o c o") 'org-clock-out)
                           (global-set-key (kbd "C-c o c j") 'org-clock-goto)

                           (add-hook 'before-save-hook 'org-align-all-tags)
                           ))
