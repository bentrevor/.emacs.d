
(progn
  (require 'package)
  (package-initialize)

  (load "~/.emacs.d/mine/settings.el")
  (load "~/.emacs.d/mine/packages.el")
  (load "~/.emacs.d/mine/keys/keybindings.el")
  (load "~/.emacs.d/mine/keyboard-macros.el")
  (load "~/.emacs.d/mine/org-mode-options.el"))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("e8a976fbc7710b60b069f27f5b2f1e216ec8d228fe5091f677717d6375d2669f" "d25d9b2b1e800a74fea4f6d174c4bd1b9c19a7617b22cc349245a36417c56ece" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "419637b7a8c9cb43f273980f0c9879c0cbadace6b38efac0281e031772c84eb2" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "c6faf8734bd04d303d9a272daef7a1c37a85597e2de3c006a4319ecf579821a0" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "90b5269aefee2c5f4029a6a039fb53803725af6f5c96036dee5dc029ff4dff60" "ce79400f46bd76bebeba655465f9eadf60c477bd671cbcd091fe871d58002a88" "9bcb8ee9ea34ec21272bb6a2044016902ad18646bd09fdd65abae1264d258d89" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "7d4d00a2c2a4bba551fcab9bfd9186abe5bfa986080947c2b99ef0b4081cb2a6" "c7359bd375132044fe993562dfa736ae79efc620f68bab36bd686430c980df1c" default)))
 '(erc-hl-nicks-mode t)
 '(erc-hl-nicks-skip-nicks (quote ("so" "So")))
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
