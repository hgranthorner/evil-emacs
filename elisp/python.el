;;; python.el --- python

;;; Commentary:
;;
;;; Code:
(require 'use-package)
(require 'general)

(defvar python-shell-interpreter "python3")
(defvar lsp-disabled-clients '(pylsp mspyls))

(use-package python
  :hook (python-mode . (lambda ()
			 (require 'lsp-pyright)
			 (lsp-deferred)))
  :init
  (setq python-indent-guess-indent-offset-verbose nil)
  (when (and (executable-find "python3")
	     (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python3"))

  :config
  (general-define-key
   :states 'motion
   :keymaps '(python-mode-map)
   :prefix grant/local-key
   "s"  '(:ignore t :wk "skeleton")
   "si" 'python-skeleton-if
   "sf" 'python-skeleton-for
   "st" 'python-skeleton-try))

(use-package pyimport
  :defer t
  :after (python)
  :config
  (general-define-key
   :states 'motion
   :keymaps 'python-mode-map
   :prefix grant/local-key
   "i" '(:ignore t :wk "imports")
   "ii" #'pyimport-insert-missing
   "ir" #'pyimport-remove-unused))

(use-package py-isort
  :defer t
  :after (python)
  :config
  (general-define-key
   :states 'motion
   :keymaps 'python-mode-map
   :prefix grant/local-key
   "i" '(:ignore t :wk "imports")
   "is" #'py-isort-buffer))

;; (use-package lsp-python-ms
;;   :ensure t
;;   :init (setq lsp-python-ms-auto-install-server t))

(use-package lsp-pyright
  :after 'lsp-mode
  :ensure t)

(use-package poetry
  :after python
  :hook (python-mode . poetry-tracking-mode)
  :config
  (poetry-tracking-mode))

(provide 'python)

;;; python.el ends here
