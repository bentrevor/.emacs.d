
(global-unset-key (kbd "M-ESC ESC"))
(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-k") 'describe-key)
(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")   'isearch-forward)
(global-set-key (kbd "C-M-r")   'isearch-backward)
(global-set-key (kbd "M-n")     'scroll-up-line)
(global-set-key (kbd "M-p")     'scroll-down-line)
(global-set-key (kbd "M-t")     'zap-up-to-char)
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-x f")   'projectile-find-file)
(global-set-key (kbd "C-x C-f") 'projectile-find-file)
(global-set-key (kbd "C-x M-f") 'find-file)
(global-set-key (kbd "M-.")     'forward-paragraph)
(global-set-key (kbd "M-,")     'backward-paragraph)
(global-set-key (kbd "M-;")     'whole-line-or-region-comment-dwim)
(global-set-key (kbd "M-o M-o") 'browse-url-at-point)
(global-set-key (kbd "C-x C-o") 'other-window)

(defun join-line-below ()
  "Like 'J' in vim."
  (interactive)
  (next-line)
  (delete-indentation))

(global-set-key (kbd "M-j") 'join-line-below)

;; (defun find-word-at-point ()
;;   "Like '*' in vim."
;;   (interactive)
;;   (backward-word)
;;   (isearch-forward-regexp)
;;   (isearch-yank-word-or-char)
;; )

;; (defun pry-rescue ()
;;   "Wrap the current line in a begin block that rescues with pry"
;;   (interactive)
;;   (move-beginning-of-line)
;;   (open-line)
;;   (indent-for-tab-command)
;;   (insert "begin")
;;   (next-line)
;;   (next-line)
;;   (move-beginning-of-line)
;;   (open-line)
;;   (indent-for-tab-command)
;;   (insert "rescue")
;;   (newline-and-indent)
;;   (insert "binding.pry")
;;   (newline-and-indent)
;;   (insert "end"))

;; (defun select-between (ch)
;;   "select region between a character"
;;   (interactive "p")
;;   (isearch-backward-regexp)
;;   (set-mark-command)
;;   (isearch-forward-regexp)
;;   (set-mark-command))

(global-set-key (kbd "M-*") 'find-word-at-point)

(defun yank-and-indent ()
  "Yank with correct indentation."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

(global-set-key (kbd "C-y") 'yank-and-indent)

(defun dotfile ()
  "Opens ~/.emacs"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun increment-next-number (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun decrement-next-number (&optional arg)
  (interactive "p*")
  (increment-next-number (if arg (- arg) -1)))
