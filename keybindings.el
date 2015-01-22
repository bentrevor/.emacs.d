
;; define my very own minor mode
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

(global-unset-key (kbd "M-ESC ESC"))
(define-key my-keys-minor-mode-map (kbd "M-/")       'hippie-expand)
(define-key my-keys-minor-mode-map (kbd "C-x C-b")   'ibuffer)
(define-key my-keys-minor-mode-map (kbd "C-x C-k")   'describe-key)
(define-key my-keys-minor-mode-map (kbd "C-s")       'isearch-forward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-r")       'isearch-backward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-M-s")     'isearch-forward)
(define-key my-keys-minor-mode-map (kbd "C-M-r")     'isearch-backward)
(define-key my-keys-minor-mode-map (kbd "M-n")       'scroll-up-line)
(define-key my-keys-minor-mode-map (kbd "M-p")       'scroll-down-line)
(define-key my-keys-minor-mode-map (kbd "M-t")       'zap-up-to-char)
(define-key my-keys-minor-mode-map (kbd "C-x r i")   'string-insert-rectangle)
(define-key my-keys-minor-mode-map (kbd "C-x a r")   'align-regexp)
(define-key my-keys-minor-mode-map (kbd "C-x f")     'projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "C-x C-f")   'projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "C-x M-f")   'find-file)
(define-key my-keys-minor-mode-map (kbd "M-.")       'forward-paragraph)
(define-key my-keys-minor-mode-map (kbd "M-,")       'backward-paragraph)
(define-key my-keys-minor-mode-map (kbd "M-;")       'whole-line-or-region-comment-dwim)
(define-key my-keys-minor-mode-map (kbd "M-o M-o")   'browse-url-at-point)
(define-key my-keys-minor-mode-map (kbd "C-x C-o")   'other-window)
(define-key my-keys-minor-mode-map (kbd "<backtab>") 'other-window)

(define-minor-mode my-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " my-keys" 'my-keys-minor-mode-map)

(my-keys-minor-mode 1)

;; make my minor mode the most important minor mode
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my keybindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'my-keys-minor-mode))
      (let ((mykeys (assq 'my-keys-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'my-keys-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mykeys))))
(ad-activate 'load)

(defun join-line-below ()
  "Like 'J' in vim."
  (interactive)
  (next-line)
  (delete-indentation))

(global-set-key (kbd "M-j") 'join-line-below)

(defvar open-paren-char ?()
(defvar closed-paren-char ?))

(defvar open-bracket-char ?{)
(defvar closed-bracket-char ?})

(defvar open-sq-bracket-char ?[)
(defvar closed-sq-bracket-char ?])

(defun on-opening-paren ()
  (let ((current-char (char-after)))
    (or (char-equal open-paren-char
                    current-char)
        (char-equal open-bracket-char
                    current-char)
        (char-equal open-sq-bracket-char
                    current-char))))

(defun on-closing-paren ()
  (let ((current-char (char-after)))
    (or (char-equal closed-paren-char
                    current-char)
        (char-equal closed-bracket-char
                    current-char)
        (char-equal closed-sq-bracket-char
                    current-char))))

(defun jump-to-opening-paren ()
  (interactive)
  (forward-char)
  (er/expand-region 1)
  (keyboard-quit))

(defun jump-to-closing-paren ()
  (interactive)
  (er/expand-region 1)
  (exchange-point-and-mark)
  (backward-char)
  (keyboard-quit))

(defun jump-to-matching-paren ()
  "Like '%' in vim."
  (interactive)
  (if (on-opening-paren)
      (jump-to-closing-paren)
      (jump-to-opening-paren)))

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
