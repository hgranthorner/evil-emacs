;;; init.el --- Initialization file for Emacs
;;; Commentary:
;   Emacs Startup File --- initialization for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install straight.el

;;; Code:

(defvar bootstrap-version)

(let ((install-url "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el")
       (bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
       (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer (url-retrieve-synchronously install-url 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install and enable use-package

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(setq native-comp-async-report-warnings-errors nil)
(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts

(set-face-attribute 'default nil :height 160)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure mac modifiers to be what you expect, and turn off the bell noise

(defvar is-mac (equal system-type 'darwin))
(if (equal system-type 'darwin)
  (setq ring-bell-function 'ignore
    mac-command-modifier 'control
    mac-option-modifier 'meta)
  (use-package exec-path-from-shell
    :init
    (exec-path-from-shell-initialize))
  (setq dired-use-ls-dired nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defaults

(defalias 'yes-or-no-p 'y-or-n-p)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-screen t)
(setq backup-directory-alist '(("." . "~/.emacs_backups")))
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(electric-pair-mode 1)
(setq lisp-indent-offset 2)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq-default indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil

(use-package evil
  :init
  (setq evil-move-beyond-eol t
        evil-normal-state-cursor '(box "black")
        evil-insert-state-cursor '(box "#98BE65")
        evil-visual-state-cursor '(box "orange")
        evil-emacs-state-cursor  '(box "purple")
        evil-undo-system         'undo-redo
        evil-want-keybinding     nil)
  :config
  (evil-mode))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package helm
  :defer f
  :init
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
    helm-move-to-line-cycle-in-source         t ; move to end or beginning of source when reaching top or bottom of source.
    helm-ff-search-library-in-sexp            t ; search for library in `require' and `declare-function' sexp.
    helm-scroll-amount                        8 ; scroll 8 lines other window using M-<next>/M-<prior>
    helm-ff-file-name-history-use-recentf     t
    helm-echo-input-in-header-line            t)
  :config
  (helm-mode 1))

(use-package helm-swoop
  :init
  (setq helm-swoop-use-fuzzy-match t
        helm-swoop-split-with-multiple-windows t))

(use-package company
  :config
  (global-company-mode))

(use-package magit)

(use-package projectile
  :init
  (setq projectile-completion-system 'helm)
  :config
  (projectile-mode 1))

(use-package flycheck
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  :config
  (global-flycheck-mode))

(load-file "~/personal-emacs/elisp/keys.el")

(provide 'init)

;;; init.el ends here
