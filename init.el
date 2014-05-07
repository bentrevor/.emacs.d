
(progn
  (require 'package)
  (package-initialize)

  ;
  ; settings
  ;
  (add-to-list 'package-archives       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'auto-mode-alist        '("\\.rb$" . ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (setq x-select-enable-clipboard            t
        x-select-enable-primary              t
        save-interprogram-paste-before-kill  t
        apropos-do-all                       t
        save-place-file (concat user-emacs-directory "places"))

  (setq-default auto-save-default nil)                ; no autosave
  (setq make-backup-files nil)                        ; no autosave
  (setq backup-directory-alist `(("." . "~/.saves"))) ; save backups in separate directory
  (setq-default indent-tabs-mode nil)                 ; use spaces instead of tabs
  (show-paren-mode 1)                                 ; highlight matching parens
  (fset 'yes-or-no-p 'y-or-n-p)                       ; faster prompts
  (setq confirm-nonexistent-file-or-buffer nil)       ; don't prompt to create a new file

  (autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
  (autoload 'zap-up-to-char "misc" "like 't' in vim" 'interactive)

  ; line number options
  (global-linum-mode t)
  (setq linum-format "%4d  ")
  (setq left-fringe 6)
  (set-face-attribute 'fringe nil :background "#FFF")

  (set-default 'truncate-lines t) ; disable word wrap

  (load-theme 'ample t)

  (require 'smex)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)

  (projectile-global-mode) ; like command-t

  (require 'flx-ido)
  (ido-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  (setq ido-create-new-buffer 'always)
  (setq projectile-completion-system 'ido)
  (setq projectile-require-project-root nil)
  ;; (setq projectile-enable-caching t)

  (menu-bar-mode -1)
  (when (fboundp 'tool-bar-mode)
    (tool-bar-mode -1))
  (when (fboundp 'scroll-bar-mode)
    (scroll-bar-mode -1))

  ;
  ; org mode options
  ;
  (load "~/.emacs.d/org-mode-options.el")

  ;
  ; options for packages
  ;
  (require 'expand-region)
  (global-set-key (kbd "M-s i") 'er/expand-region)

  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)

  (require 'saveplace)
  (setq-default save-place t)

  (require 'whole-line-or-region)
  (whole-line-or-region-mode)

  ;
  ; clojure indent options
  ;
  (add-hook 'clojure-mode-hook
	    (lambda ()
	      (put-clojure-indent 'describe 'defun)
	      (put-clojure-indent 'it       'defun)
	      (put-clojure-indent 'with     'defun)))

  ;
  ; keybindings
  ;
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

  (defun yank-and-indent ()
    "Yank with correct indentation."
    (interactive)
    (yank)
    (call-interactively 'indent-region))

  (global-set-key (kbd "\C-y") 'yank-and-indent)

  (defun dotfile ()
    "Opens ~/.emacs"
    (interactive)
    (find-file "~/.emacs.d/.emacs"))

  (define-key global-map (kbd "RET") 'newline-and-indent))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("c6faf8734bd04d303d9a272daef7a1c37a85597e2de3c006a4319ecf579821a0" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" default)))
 '(org-agenda-files (quote ("~/org/todo.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
