;; "builtin" includes functions defined in packages

;; unset
(global-unset-key (kbd "M-ESC ESC"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x C-o"))
(global-unset-key (kbd "C-x 0"))
(global-unset-key (kbd "C-x 1"))
(global-unset-key (kbd "C-x 2"))
(global-unset-key (kbd "C-x 3"))
(global-unset-key (kbd "C-x m"))
(define-key my-keys-minor-mode-map (kbd "C-c C-h")   '(lambda () (interactive))) ;; global-unset-key wasn't working

(define-key my-keys-minor-mode-map (kbd "C-s")       'isearch-forward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-r")       'isearch-backward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-h")       'delete-backward-char)

;; karabiner remaps vt102 control codes:
;; C-' to C-\
;; C-/ to C-_
;; C-, to C-]
;; C-. to C-^
(define-key my-keys-minor-mode-map (kbd "C-]")       'backward-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-^")       'forward-paragraph)

(define-key my-keys-minor-mode-map (kbd "M-,")       'beginning-of-buffer)
(define-key my-keys-minor-mode-map (kbd "M-.")       'end-of-buffer)

(define-key my-keys-minor-mode-map (kbd "C-w")       'backward-kill-word)

(define-key my-keys-minor-mode-map (kbd "C-v")       'whole-line-or-region-kill-region)
(define-key my-keys-minor-mode-map (kbd "M-v")       'whole-line-or-region-kill-ring-save)

;; C-x
(define-key my-keys-minor-mode-map (kbd "C-x d k")   'describe-key)
(define-key my-keys-minor-mode-map (kbd "C-x C-b")   'ibuffer)
(define-key my-keys-minor-mode-map (kbd "C-x f")     'projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "C-x C-f")   'projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "C-x M-f")   'find-file)
(define-key my-keys-minor-mode-map (kbd "C-x z")     'delete-other-windows)
(define-key my-keys-minor-mode-map (kbd "C-x /")     'winner-undo)
(define-key my-keys-minor-mode-map (kbd "C-x \\")    'split-window-right)
(define-key my-keys-minor-mode-map (kbd "C-x C-\\")  'split-window-right)
(define-key my-keys-minor-mode-map (kbd "C-x -")     'split-window-below)
(define-key my-keys-minor-mode-map (kbd "C-x C-d")   'delete-window)

;; move cursor between windows
(define-key my-keys-minor-mode-map (kbd "C-x h")     'windmove-left)
(define-key my-keys-minor-mode-map (kbd "C-x j")     'windmove-down)
(define-key my-keys-minor-mode-map (kbd "C-x k")     'windmove-up)
(define-key my-keys-minor-mode-map (kbd "C-x l")     'windmove-right)
(define-key my-keys-minor-mode-map (kbd "C-x C-h")   'windmove-left)
(define-key my-keys-minor-mode-map (kbd "C-x C-j")   'windmove-down)
(define-key my-keys-minor-mode-map (kbd "C-x C-k")   'windmove-up)
(define-key my-keys-minor-mode-map (kbd "C-x C-l")   'windmove-right)

;; other
(define-key my-keys-minor-mode-map (kbd "C-x g")     'magit-status)
(define-key my-keys-minor-mode-map (kbd "<backtab>") 'hippie-expand)
(define-key my-keys-minor-mode-map (kbd "C-c M-t")   'transpose-words)
(define-key my-keys-minor-mode-map (kbd "M-;")       'whole-line-or-region-comment-dwim)
(define-key my-keys-minor-mode-map (kbd "C-x u")     'browse-url-at-point)

;; s == scroll
(smartrep-define-key
    my-keys-minor-mode-map "C-c s" '(("n" . 'scroll-up-line)
                                     ("p" . 'scroll-down-line)
                                     ))

;; yank without auto-indentation
(define-key my-keys-minor-mode-map (kbd "M-C-y") 'yank)

;; yank-and-indent is annoying in haskell
(add-hook 'haskell-mode-hook (lambda () (define-key my-keys-minor-mode-map (kbd "C-y") 'yank)))
