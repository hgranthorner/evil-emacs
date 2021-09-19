;;; python.el --- python

;;; Commentary:
;;
;;; Code:
(require 'use-package)

(use-package python
  :config
  (add-hook 'python-mode-hook #'lsp-deferred))

(use-package poetry
  :after python
  :config
  (poetry-tracking-mode))

(provide 'python)

;;; python.el ends here
