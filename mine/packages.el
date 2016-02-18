(autoload 'zap-up-to-char "misc" "like 't' in vim" 'interactive)
(autoload 'ruby-mode "ruby-mode" "Major mode for ruby files" t)
(autoload 'nimrod-mode "nimrod-mode" "Major mode for nim files" t)

(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(setq web-mode-markup-indent-offset 2) ;; html indent
(setq web-mode-code-indent-offset 2) ;; js indent

(add-to-list 'auto-mode-alist '("zshenv" . shell-script-mode))
(add-to-list 'auto-mode-alist '("zshrc" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

(add-to-list 'auto-mode-alist '("\\.purs$" . haskell-mode))

;; for nand2tetris
(add-to-list 'auto-mode-alist '("\\.hdl$" . c-mode))

(add-to-list 'auto-mode-alist '("\\.nim$" . nimrod-mode))

(require 'smex)
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

(projectile-global-mode) ;; like command-t

(font-lock-add-keywords 'javascript-mode
                        '(("Math" . font-lock-type-face)
                          ("Number" . font-lock-type-face)
                          ("Date" . font-lock-type-face)
                          ("String" . font-lock-type-face)
                          ("RegExp" . font-lock-type-face)
                          ("Array" . font-lock-type-face)
                          ("JSON" . font-lock-type-face)
                          ("Object" . font-lock-type-face)
                          ))

(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; vertical minibuffer
(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(add-hook 'ido-minibuffer-setup-hook (lambda ()
                                       (set (make-local-variable 'truncate-lines) nil)))
(add-hook 'ido-setup-hook (lambda ()
                            (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
                            (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)))
(setq ido-create-new-buffer 'always)
(setq projectile-completion-system 'ido)
(setq projectile-require-project-root nil)
(setq projectile-indexing-method 'alien)
(setq projectile-globally-ignored-directories '("node_modules"))
;; (setq projectile-enable-caching t)

(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(require 'expand-region)
(global-set-key (kbd "M-s i") 'er/expand-region)
(global-set-key (kbd "M-s M-i") 'er/expand-region)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(require 'saveplace)
(setq-default save-place t)

(require 'whole-line-or-region)
(whole-line-or-region-mode)

;; for easy keybindings with single-key repeats
(require 'smartrep)
