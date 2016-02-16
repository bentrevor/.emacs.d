;; define my very own minor mode
(defvar my-keys-minor-mode-map (make-keymap) "my-keys-minor-mode keymap.")

;; unset
(global-unset-key (kbd "M-ESC ESC"))
(global-unset-key (kbd "C-x o"))
(global-unset-key (kbd "C-x C-o"))
(global-unset-key (kbd "C-x 0"))
(global-unset-key (kbd "C-x 1"))
(global-unset-key (kbd "C-x 2"))
(global-unset-key (kbd "C-x 3"))
(define-key my-keys-minor-mode-map (kbd "C-c C-h")   '(lambda () (interactive))) ;; global-unset-key wasn't working

;; C-
(define-key my-keys-minor-mode-map (kbd "C-s")       'isearch-forward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-r")       'isearch-backward-regexp)
(define-key my-keys-minor-mode-map (kbd "C-h")       'delete-backward-char)

;; M-
(define-key my-keys-minor-mode-map (kbd "M-t")       'zap-up-to-char)
(define-key my-keys-minor-mode-map (kbd "M-.")       'forward-paragraph)
(define-key my-keys-minor-mode-map (kbd "M-,")       'backward-paragraph)
(define-key my-keys-minor-mode-map (kbd "M-;")       'whole-line-or-region-comment-dwim)
(define-key my-keys-minor-mode-map (kbd "M-o M-o")   'browse-url-at-point)

(define-key my-keys-minor-mode-map (kbd "C-w")       'backward-kill-word)
(define-key my-keys-minor-mode-map (kbd "M-w")       'forward-to-beginning-of-next-word)

(define-key my-keys-minor-mode-map (kbd "C-v")       'whole-line-or-region-kill-region)
(define-key my-keys-minor-mode-map (kbd "M-v")       'whole-line-or-region-kill-ring-save)

;; C-x
(define-key my-keys-minor-mode-map (kbd "C-x d k")   'describe-key)
(define-key my-keys-minor-mode-map (kbd "C-x C-b")   'ibuffer)
(define-key my-keys-minor-mode-map (kbd "C-x f")     'projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "C-x C-f")   'projectile-find-file)
(define-key my-keys-minor-mode-map (kbd "C-x M-f")   'find-file)
(define-key my-keys-minor-mode-map (kbd "C-x C-y")   'pbcopy)
;; tmux-like
(define-key my-keys-minor-mode-map (kbd "C-x C-o")   'switch-to-previous-buffer)
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

;; uncategorized
(define-key my-keys-minor-mode-map (kbd "<backtab>") 'hippie-expand)
(define-key my-keys-minor-mode-map (kbd "C-c M-t")   'transpose-words)

;; repeating

;; s == scroll
(smartrep-define-key
    my-keys-minor-mode-map "C-c s" '(("n" . 'scroll-up-line)
                                     ("p" . 'scroll-down-line)
                                     ))

;; n == number
(smartrep-define-key
    my-keys-minor-mode-map "C-c n" '(("p" . 'increment-next-number)
                                     ("n" . 'decrement-next-number)
                                     ))

;; paredit mode
(add-hook 'paredit-mode-hook
          (lambda ()
            (define-key my-keys-minor-mode-map (kbd "C-c p u") 'paredit-splice-sexp) ;; unwrap
            (define-key my-keys-minor-mode-map (kbd "C-c p s") 'paredit-split-sexp)
            (define-key my-keys-minor-mode-map (kbd "C-c p j") 'paredit-join-sexp)

            (define-key my-keys-minor-mode-map (kbd "C-c p i") 'paredit-insert-text)

            (define-key my-keys-minor-mode-map (kbd "C-c p b s") 'paredit-backward-slurp-sexp)
            (define-key my-keys-minor-mode-map (kbd "C-c p f s") 'paredit-forward-slurp-sexp)
            (define-key my-keys-minor-mode-map (kbd "C-c p b b") 'paredit-backward-barf-sexp)
            (define-key my-keys-minor-mode-map (kbd "C-c p f b") 'paredit-forward-barf-sexp)

            (global-paren-face-mode t)
            ))

;; defined below ;;
;;;;;;;;;;;;;;;;;;;
(define-key my-keys-minor-mode-map (kbd "C-y") 'yank-and-indent)
(define-key my-keys-minor-mode-map (kbd "M-C-y") 'yank)
(define-key my-keys-minor-mode-map (kbd "M-C-t") 'kill-to-string)
(define-key my-keys-minor-mode-map (kbd "M-j") 'join-line-below)
(define-key my-keys-minor-mode-map (kbd "C-]") 'jump-to-matching-paren)
(define-key my-keys-minor-mode-map (kbd "C-c %") 'jump-to-matching-paren)

;; yank-and-indent is annoying in haskell
(add-hook 'haskell-mode-hook (lambda () (define-key my-keys-minor-mode-map (kbd "C-y") 'yank)))

;; C-c vim stuff
(define-key my-keys-minor-mode-map (kbd "C-c ;")     'open-prompt)

(define-key my-keys-minor-mode-map (kbd "C-c d a p") 'delete-around-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-c d a w") 'delete-around-word)
(define-key my-keys-minor-mode-map (kbd "C-c d a b") 'delete-around-parens)
(define-key my-keys-minor-mode-map (kbd "C-c d a B") 'delete-around-curly-braces)
(define-key my-keys-minor-mode-map (kbd "C-c d i p") 'delete-in-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-c d i w") 'delete-in-word)
(define-key my-keys-minor-mode-map (kbd "C-c d i b") 'delete-in-parens)
(define-key my-keys-minor-mode-map (kbd "C-c d i B") 'delete-in-curly-braces)

(define-key my-keys-minor-mode-map (kbd "C-c y a p") 'copy-around-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-c y a w") 'copy-around-word)
(define-key my-keys-minor-mode-map (kbd "C-c y a b") 'copy-around-parens)
(define-key my-keys-minor-mode-map (kbd "C-c y a B") 'copy-around-curly-braces)
(define-key my-keys-minor-mode-map (kbd "C-c y i p") 'copy-in-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-c y i w") 'copy-in-word)
(define-key my-keys-minor-mode-map (kbd "C-c y i b") 'copy-in-parens)
(define-key my-keys-minor-mode-map (kbd "C-c y i B") 'copy-in-curly-braces)

(define-key my-keys-minor-mode-map (kbd "C-c v a p") 'select-around-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-c v a w") 'select-around-word)
(define-key my-keys-minor-mode-map (kbd "C-c v a b") 'select-around-parens)
(define-key my-keys-minor-mode-map (kbd "C-c v a B") 'select-around-curly-braces)
(define-key my-keys-minor-mode-map (kbd "C-c v i p") 'select-in-paragraph)
(define-key my-keys-minor-mode-map (kbd "C-c v i w") 'select-in-word)
(define-key my-keys-minor-mode-map (kbd "C-c v i b") 'select-in-parens)
(define-key my-keys-minor-mode-map (kbd "C-c v i B") 'select-in-curly-braces)

(defconst vim-fn-commands '(("d" "delete") ("y" "copy") ("v" "select")))
(defconst vim-fn-scopes '(("i" "in") ("a" "around")))
(defconst vim-fn-boundaries '(("p" "paragraph") ("w" "word") ("b" "parens") ("B" "curly-braces") ("[" "square-brackets")))

;; each command
;;   each scope
;;     each boundary
;;       (define-key my-keys-minor-mode-map (kbd (concat "C-c " command scope boundary)) '(TODO))

(defun delete-around-paragraph ()    (interactive) (vim-function "delete" "around" "paragraph"))
(defun delete-around-word ()         (interactive) (vim-function "delete" "around" "word"))
(defun delete-around-parens ()       (interactive) (vim-function "delete" "around" "parens"))
(defun delete-around-curly-braces () (interactive) (vim-function "delete" "around" "curly-braces"))
(defun delete-in-paragraph ()        (interactive) (vim-function "delete" "in" "paragraph"))
(defun delete-in-word ()             (interactive) (vim-function "delete" "in" "word"))
(defun delete-in-parens ()           (interactive) (vim-function "delete" "in" "parens"))
(defun delete-in-curly-braces ()     (interactive) (vim-function "delete" "in" "curly-braces"))

(defun copy-around-paragraph ()      (interactive) (vim-function "copy" "around" "paragraph"))
(defun copy-around-word ()           (interactive) (vim-function "copy" "around" "word"))
(defun copy-around-parens ()         (interactive) (vim-function "copy" "around" "parens"))
(defun copy-around-curly-braces ()   (interactive) (vim-function "copy" "around" "curly-braces"))
(defun copy-in-paragraph ()          (interactive) (vim-function "copy" "in" "paragraph"))
(defun copy-in-word ()               (interactive) (vim-function "copy" "in" "word"))
(defun copy-in-parens ()             (interactive) (vim-function "copy" "in" "parens"))
(defun copy-in-curly-braces ()       (interactive) (vim-function "copy" "in" "curly-braces"))

(defun select-around-paragraph ()    (interactive) (vim-function "select" "around" "paragraph"))
(defun select-around-word ()         (interactive) (vim-function "select" "around" "word"))
(defun select-around-parens ()       (interactive) (vim-function "select" "around" "parens"))
(defun select-around-curly-braces () (interactive) (vim-function "select" "around" "curly-braces"))
(defun select-in-paragraph ()        (interactive) (vim-function "select" "in" "paragraph"))
(defun select-in-word ()             (interactive) (vim-function "select" "in" "word"))
(defun select-in-parens ()           (interactive) (vim-function "select" "in" "parens"))
(defun select-in-curly-braces ()     (interactive) (vim-function "select" "in" "curly-braces"))

(defun switch-to-previous-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun paredit-insert-text (txt)
  "ignore paredit rules and just insert text"
  (interactive "stext: ")
  (insert txt))

(defun open-prompt (command)
  "like pressing : in vim."
  (interactive "s:")
  (let ((last-char (substring command -1))
        (line-no (string-to-number (substring command 0 -1))))
    (cond
     ((string= last-char "d") (delete-line-by-number line-no))
     ((string= last-char "y") (copy-line-by-number line-no))
     ((string= last-char "p") (yank-line-by-number line-no))
     ((< 0 (string-to-number command)) (goto-line (string-to-number command)))
     (t (message "invalid!")))
    (save-buffer)))

(defun vim-function (command boundaries scope)
  (vim-select boundaries scope)

  (cond
   ((string= command "copy") (whole-line-or-region-kill-ring-save 1))
   ((string= command "select") nil) ;; region will always be selected by this point
   ((string= command "delete") (whole-line-or-region-kill-region 1)))

  (message (concat command " " boundaries " " scope)))

(defun vim-select (boundaries scope)
  (cond
   ((string= boundaries "in") (vim-select-in scope))
   ((string= boundaries "around") (vim-select-around scope))))

(defun vim-select-in (scope)
  (cond
   ((string= scope "word") (er/expand-region 1))
   ((string= scope "paragraph") (mark-paragraph) (forward-char))
   ((string= scope "parens") (select-in "("))
   ((string= scope "curly-braces") (select-in "{"))
   ((string= scope "square-brackets") (select-in "["))
   ))

(defun vim-select-around (scope)
  (cond
   ((string= scope "word") (er/expand-region 1)) ;; TODO this works the same as "in" right now (but I never used `daw` much anyway)
   ((string= scope "paragraph") (mark-paragraph))
   ((string= scope "parens") (select-around "("))
   ((string= scope "curly-braces") (select-around "{"))
   ((string= scope "square-brackets") (select-around "["))
   ))

(defun select-in (char)
  (search-backward char)
  (er/expand-region 1)
  (forward-char)
  (exchange-point-and-mark)
  (backward-char)
  )

(defun select-around (char)
  (search-backward char)
  (er/expand-region 1)
  )

(defalias 'rectangle-insert 'string-insert-rectangle)
(defalias 'rectangle-replace 'string-rectangle)

(defun pbcopy ()
  (interactive)
  (shell-command-on-region (region-beginning) (region-end) "pbcopy")
  (deactivate-mark))

(defun forward-to-beginning-of-next-word ()
  "Like 'w' in vim."
  (interactive)
  (if (= 0 (skip-chars-forward "^a-zA-Z"))
      ((lambda ()
          (forward-word)
          (forward-word)
          (backward-word)))))

(defun delete-line-by-number (line-number)
  "Like :<n>d in vim."
  (goto-line line-number)
  (whole-line-or-region-kill-region 1)
  (exchange-point-and-mark))

(defun copy-line-by-number (line-number)
  "Like :<n>y in vim."
  (goto-line line-number)
  (whole-line-or-region-kill-ring-save 1)
  (exchange-point-and-mark))

(defun yank-line-by-number (line-number)
  "Like :<n>p in vim."
  (goto-line line-number)
  (yank-and-indent)
  (exchange-point-and-mark))

(defun kill-to-string (target)
  (interactive "skill to string: ")
  (set-mark-command nil)
  (search-forward target)
  (search-backward target)
  (kill-region (region-beginning) (region-end)))

(defun join-line-below ()
  "Like 'J' in vim."
  (interactive)
  (next-line)
  (delete-indentation))

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

(defun nearest-half (x)
  (/ (round (* 2 x)) 2.0))

(defun insert-pert (nums)
  (interactive "sestimates: ")
  (let* ((strs (split-string nums " "))
         (low (nth 0 strs))
         (medium (nth 1 strs))
         (high (nth 2 strs))
         (pert (/ (+ (string-to-number low) (* 4 (string-to-number medium)) (string-to-number high)) 6.0))
         (rounded-pert (number-to-string (nearest-half pert)))
         )
    (insert (concat rounded-pert "(" low "," medium "," high ")"))))

(defun jump-to-opening-paren ()
  (forward-char)
  (er/expand-region 1)
  (keyboard-quit)
  (backward-char))

(defun jump-to-closing-paren ()
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

;; (global-set-key (kbd "M-*") 'find-word-at-point)

(defun yank-and-indent ()
  "Yank with correct indentation."
  (interactive)
  (yank)
  (call-interactively 'indent-region))

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

(defvar first-names '("mary" "patricia" "linda" "barbara" "elizabeth" "sarah" "kimberly" "deborah" "jessica" "shirley"
                      "cynthia" "angela" "melissa" "brenda" "amy" "anna" "rebecca" "virginia" "kathleen" "pamela" "martha"
                      "veronica" "jill" "erin" "geraldine" "lauren" "cathy" "joann" "lorraine" "lynn" "sally" "regina"
                      "erica" "beatrice" "dolores" "bernice" "audrey" "yvonne" "annette" "june" "samantha" "marion" "dana"
                      "stacy" "ana" "renee" "ida" "vivian" "roberta" "holly" "brittany" "melanie" "loretta" "yolanda"
                      "jeanette" "laurie" "katie" "kristen" "james" "john" "robert" "michael" "william" "david" "richard"
                      "charles" "joseph" "thomas" "christopher" "daniel" "paul" "mark" "donald" "george" "kenneth"
                      "steven" "edward" "brian" "ronald" "anthony" "kevin" "jason" "matthew" "gary" "timothy" "jose"
                      "larry" "jeffrey" "frank" "scott" "eric" "stephen" "andrew" "raymond" "gregory" "joshua" "jerry"
                      "dennis" "walter" "patrick" "peter" "harold" "douglas" "henry" "carl" "arthur" "ryan" "roger" "joe"
                      "bernard" "mario" "leroy" "francisco" "marcus" "micheal" "theodore" "clifford" "miguel" "oscar"
                      "jay" "jim" "tom" "calvin" "alex" "jon" "ronnie" "bill" "lloyd" "tommy"))

(defvar last-names '("aquirre" "arscott" "bagent" "balin" "bernales" "bomkamp" "bouthillier" "cavendish" "clyatt"
                     "detienne" "dewbre" "dimuro" "dingledine" "dosh" "droney" "dunklee" "duyck" "emilio" "ence"
                     "eversley" "fetzner" "garofano" "gellis" "gemmer" "grealish" "haertel" "haik" "handyside"
                     "haroutunian" "hornburg" "hurtig" "jenniges" "juncaj" "kallhoff" "kanaan" "kleban" "klontz" "knier"
                     "kopischke" "kugelman" "kuri" "lacoss" "lamarque" "langsdorf" "latouche" "leabo" "lorette" "maracle"
                     "mathus" "mccamy" "merta" "meulemans" "montieth" "muoio" "neyens" "niccoli" "oberhaus"
                     "oborn" "osorto" "penkala" "podoll" "prenatt" "ramone" "romes" "roupp" "ruscitti" "santaella"
                     "scozzari" "siverling" "speigner" "spinnato" "stentz" "stocke" "sundt" "thorup" "tresch" "tripplett"
                     "uhls" "urdaneta" "uttech" "vosler" "werber" "wieand" "zacharia" "zeleznik" "zoucha" "zuch"))

(defun select-random (coll)
  (let ((selected (nth (random (length coll)) coll)))
    (either selected
            (capitalize selected))))

(defun either (a b)
  (if (= 1 (random 2)) a b))

(defun run-erc ()
  (interactive)
  (let ((nickname (concat (select-random first-names)
                          (either (concat "_" (select-random last-names))
                                  (number-to-string (+ 1000 (random 10000)))
                                  ))))
    (erc :server "irc.freenode.net"
         :port 6667
         :nick nickname
         )))

(defun just-text ()
  (setq mode-line-format nil)
  (global-linum-mode 0)
  (define-key my-keys-minor-mode-map (kbd "C-x C-s") (lambda () (interactive) (message "can't save in notes mode")))
  (setq initial-scratch-message "")
  )

;; mode ;;
;;;;;;;;;;
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
