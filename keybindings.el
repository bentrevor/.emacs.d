
(global-unset-key (kbd "M-ESC ESC"))
(global-set-key (kbd "M-/")     'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-s")     'isearch-forward-regexp)
(global-set-key (kbd "C-r")     'isearch-backward-regexp)
(global-set-key (kbd "C-M-s")   'isearch-forward)
(global-set-key (kbd "C-M-r")   'isearch-backward)
(global-set-key (kbd "M-n")     'scroll-up-line)
(global-set-key (kbd "M-p")     'scroll-down-line)
(global-set-key (kbd "M-z")     'zap-up-to-char)
(global-set-key (kbd "C-x r i") 'string-insert-rectangle)
(global-set-key (kbd "C-x a r") 'align-regexp)
(global-set-key (kbd "C-x f")   'projectile-find-file)
(global-set-key (kbd "C-x C-f") 'projectile-find-file)
(global-set-key (kbd "M-.")     'forward-paragraph)
(global-set-key (kbd "M-,")     'backward-paragraph)
(global-set-key (kbd "M-;")     'whole-line-or-region-comment-dwim)

(defun join-line-below ()
  "Like 'J' in vim."
  (interactive)
  (next-line)
  (delete-indentation))

(global-set-key (kbd "M-j") 'join-line-below)

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
