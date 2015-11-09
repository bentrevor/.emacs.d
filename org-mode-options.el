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

(add-hook 'org-mode-hook (lambda ()
                           (linum-mode 0) ;; disable line numbers
                           (global-linum-mode 0) ;; disable line numbers
                           (visual-line-mode)    ;; wrap long lines

                           (defun org-todo-subtree ()
                             (interactive)
                             ;; mark beginning of first task heading
                             (beginning-of-line) (next-line) (forward-word) (backward-word) (set-mark-command nil)
                             ;; go back up to checklist heading and jump to next checklist
                             (previous-line) (org-forward-heading-same-level 1)
                             ;; put cursor at beginning of last task heading and insert TODO
                             (previous-line) (forward-word) (backward-word) (string-insert-rectangle (region-beginning) (region-end) "TODO ")
                             (deactivate-mark))

                           ;; a == agenda
                           ;; (just a todo list, not what org mode considers an "agenda")
                           (global-set-key (kbd "C-c o a a") (lambda () (interactive) (org-small-agenda-list ""))) ; all
                           (global-set-key (kbd "C-c o a t") (lambda () (interactive) (org-small-agenda-list "TODO")))
                           (global-set-key (kbd "C-c o a d") (lambda () (interactive) (org-small-agenda-list "DONE")))

                           ;; i == insert
                           (global-set-key (kbd "C-c o i p") 'org-insert-property)

                           ;; t == todo
                           (smartrep-define-key
                               global-map "C-c o t" '(("t" . (lambda () (interactive) (org-todo "TODO")))
                                                      ("d" . (lambda () (interactive) (org-todo "DONE")))
                                                      ("l" . (lambda () (interactive) (org-todo "LATER")))
                                                      ("s" . 'org-todo-subtree)
                                                      ("C-n" . 'outline-next-visible-heading)
                                                      ("C-p" . 'outline-previous-visible-heading)))

                           ;; l == link
                           (global-set-key (kbd "C-c o l c") 'org-store-link) ; copy
                           (global-set-key (kbd "C-c o l p") 'org-insert-link) ; paste
                           (global-set-key (kbd "C-c o l f") 'org-open-at-point) ; follow

                           ;; m s == move subtree
                           (smartrep-define-key
                              global-map "C-c o m s" '(("b" . 'org-promote-subtree)
                                                       ("f" . 'org-demote-subtree)
                                                       ("C-n" . 'next-line)
                                                       ("C-p" . 'previous-line)
                                                       ("p" . 'org-metaup)
                                                       ("n" . 'org-metadown)))

                           ;; d == date
                           (smartrep-define-key
                               global-map "C-c o d" '(("b" . 'org-timestamp-down-day)
                                                      ("f" . 'org-timestamp-up-day)
                                                      ("p" . 'org-timestamp-up)
                                                      ("n" . 'org-timestamp-down)
                                                      ("m" . 'org-date-change-minutes)
                                                      ("h" . 'org-date-change-hours)))

                           (global-set-key (kbd "C-c o d c") 'org-date-change) ; change

                           ;; c == clock
                           (global-set-key (kbd "C-c o c i") 'org-clock-in)
                           (global-set-key (kbd "C-c o c o") 'org-clock-out)
                           (global-set-key (kbd "C-c o c j") 'org-clock-goto)

                           (add-hook 'before-save-hook 'org-align-all-tags)))
