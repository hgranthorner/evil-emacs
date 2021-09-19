;;; go.el --- go

;;; Commentary:
;;
;;; Code:
(require 'use-package)
(require 'lsp)

(defun lsp-go-install-save-hooks ()
  "Set up hooks to be called on save, in go mode."
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))


(use-package go-mode
  :config
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks))


(provide 'go)

;;; go.el ends here
