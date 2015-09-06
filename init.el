
(progn
  (require 'package)
  (package-initialize)

  ;;
  ;; settings
  ;;
  (add-to-list 'package-archives       '("melpa" . "http://melpa.milkbox.net/packages/") t)
  (add-to-list 'auto-mode-alist        '("\\.rb$" . ruby-mode))
  (add-to-list 'interpreter-mode-alist '("ruby" . ruby-mode))
  (setq ruby-insert-encoding-magic-comment nil)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  (add-hook 'prog-mode-hook 'highlight-numbers-mode)
  ;; (add-hook 'prog-mode-hook 'highlight-escape-sequences)

  (setq x-select-enable-clipboard            t
        x-select-enable-primary              t
        save-interprogram-paste-before-kill  t
        apropos-do-all                       t
        save-place-file (concat user-emacs-directory "places"))

  (setq-default auto-save-default nil)                    ;; no autosave
  (setq make-backup-files nil)                            ;; no autosave
  (setq backup-directory-alist `(("." . "~/.saves")))     ;; save backups in separate directory
  (setq-default indent-tabs-mode nil)                     ;; use spaces instead of tabs
  (setq-default coffee-tab-width 2)                       ;; coffee mode tab width
  (setq-default js-indent-level 2)                        ;; js mode tab width
  (setq-default c-basic-offset 8)                         ;; c mode tab width
  (show-paren-mode 1)                                     ;; highlight matching parens
  (fset 'yes-or-no-p 'y-or-n-p)                           ;; faster prompts
  (setq confirm-nonexistent-file-or-buffer nil)           ;; don't prompt to create a new file
  (define-key global-map (kbd "RET") 'newline-and-indent) ;; indent after pressing 'enter'
  (setq set-mark-command-repeat-pop 1)                    ;; can press C-space to cycle through mark ring (after pressing C-x C-space)
  (setq-default scss-compile-at-save nil)                 ;; don't try to compile scss files
  (setq-default auto-compression-mode 0)                  ;; don't try to decompress files (like ~/.z)
  (setq-default fill-column 100)                          ;; text width for fill-paragraph
  (setq-default comment-column 0)                         ;; stop moving comments to the right
  (column-number-mode t)                                  ;; show column number
  (set-default 'truncate-lines t)                         ;; disable word wrap
  (setq ruby-deep-indent-paren nil)                       ;; better indentation for multiline hashes in ruby
  (winner-mode 1)                                         ;; undo/redo split layout changes

  (setq erc-hide-list '("JOIN" "PART" "QUIT")) ;; don't show "xxx joined the channel"
  (setq erc-input-line-position -2)  ;; supposed to keep newest message at bottom of buffer, but it's not working...


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

  (add-to-list 'auto-mode-alist '("zshrc" . shell-script-mode))
  (add-to-list 'auto-mode-alist '("\\.zsh$" . shell-script-mode))

  (add-to-list 'auto-mode-alist '("\\.purs$" . haskell-mode))


  ;; for nand2tetris
  (add-to-list 'auto-mode-alist '("\\.hdl$" . c-mode))

  (add-to-list 'auto-mode-alist '("\\.nim$" . nimrod-mode))

  ;; line number options
  (global-linum-mode t)
  (setq linum-format "%4d  ")
  (setq left-fringe 6)

  ;; inverted colors
  ;; (set-face-attribute 'linum nil :background "#dedede" :foreground "#888888")
  ;; (load-theme 'whiteboard t)
  ;; (let ((class '((class color) (min-colors 89))))
  ;;   (custom-theme-set-faces 'whiteboard
  ;;                           `(default                          ((,class (:background "whitesmoke" :foreground "white"))))
  ;;                           `(font-lock-comment-delimiter-face ((,class (:foreground "gray"))))
  ;;                           `(font-lock-comment-face           ((,class (:foreground "gray"))))
  ;;                           ))

  ;; normal colors
  (load-theme 'ample t)
  (set-face-attribute 'fringe nil :background "#FFF")

  ;;
  ;; options for packages
  ;;
  (require 'smex)
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)

  (projectile-global-mode) ;; like command-t

  (autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
  (add-hook 'emacs-lisp-mode-hook                  #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'ielm-mode-hook                        #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook                        #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook            #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook                      #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook                     #'enable-paredit-mode)
  (add-hook 'racket-mode-hook                      #'enable-paredit-mode)
  (setq racket-mode-pretty-lambda nil) ;; prevents emacs from showing 'lambda' as 'λ'

  (font-lock-add-keywords 'scheme-mode
                          '(("car " . font-lock-builtin-face)
                            ("cdr " . font-lock-builtin-face)
                            ("cons " . font-lock-builtin-face)
                            ))

  (put 'if 'scheme-indent-function 3)

  (add-hook 'paredit-mode-hook (lambda ()
                                 (global-set-key (kbd "C-c p s")   'paredit-splice-sexp)
                                 (global-set-key (kbd "C-c p b s") 'paredit-backward-slurp-sexp)
                                 (global-set-key (kbd "C-c p f s") 'paredit-forward-slurp-sexp)
                                 (global-set-key (kbd "C-c p b b") 'paredit-backward-barf-sexp)
                                 (global-set-key (kbd "C-c p f b") 'paredit-forward-barf-sexp)
                                 (global-paren-face-mode t)
                                 ))

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

  ;; ;; helm
  ;; (add-to-list 'load-path "~/.emacs.d/elpa/emacs-async")
  ;; (add-to-list 'load-path "~/.emacs.d/elpa/helm")
  ;; (require 'helm-config)

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

  ;;
  ;; hooks
  ;;
  (add-hook 'clojure-mode-hook
	    (lambda ()
	      (put-clojure-indent 'describe 'defun)
	      (put-clojure-indent 'it       'defun)
	      (put-clojure-indent 'with     'defun)))

  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)

  (add-hook 'erc-mode-hook '(lambda () (setq scroll-conservatively 100)))

  (add-hook 'go-mode-hook
            (lambda ()
              (setq gofmt-command "goimports")  ;; use goimports instead of go-fmt
              (add-hook 'before-save-hook 'gofmt-before-save)))

  (add-hook 'markdown-mode-hook
            (lambda ()
              (remove-hook 'before-save-hook 'delete-trailing-whitespace)))

  ;; (add-hook 'ido-mode-hook
  ;;           (lambda ()
  ;;             (global-set-key (kbd "C-j") 'ido-exit-minibuffer)))

  (load "~/.emacs.d/keybindings.el")
  (load "~/.emacs.d/org-mode-options.el"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("d25d9b2b1e800a74fea4f6d174c4bd1b9c19a7617b22cc349245a36417c56ece" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "419637b7a8c9cb43f273980f0c9879c0cbadace6b38efac0281e031772c84eb2" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "c6faf8734bd04d303d9a272daef7a1c37a85597e2de3c006a4319ecf579821a0" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" default)))
 '(erc-modules
   (quote
    (autojoin button completion fill irccontrols list match menu move-to-prompt netsplit networks noncommands readonly ring scrolltobottom stamp track)))
 '(safe-local-variable-values (quote ((encoding . utf-8)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))
