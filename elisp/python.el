;;; python.el --- python

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package python
  :hook (python-mode . lsp-deferred)
  :general
  (:states 'motion
    :keymaps '(python-mode-map)
    :prefix grant/local-key
    "s"  '(:ignore t :wk "skeleton")
    "si" 'python-skeleton-if
    "sf" 'python-skeleton-for
    "st" 'python-skeleton-try))

(use-package poetry
  :after python
  :config
  (poetry-tracking-mode))

(provide 'python)

;;; python.el ends here
