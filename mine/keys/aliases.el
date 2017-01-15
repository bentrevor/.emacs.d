
(defalias 'insert-rectangle 'string-insert-rectangle)
(defalias 'replace-rectangle 'string-rectangle)

(defalias 'git 'magit-status)
(defalias 'blame 'magit-blame)

(advice-add 'magit-status :before #'delete-other-windows)
