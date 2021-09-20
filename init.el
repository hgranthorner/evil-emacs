;;; init.el --- Initialization file for Emacs
;;; Commentary:
;;   Emacs Startup File --- initialization for Emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install straight.el

;;; Code:
(setq gc-cons-threshold 100000000)
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
(setq use-package-always-ensure t)
(setq native-comp-async-report-warnings-errors nil)

(require 'use-package)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Fonts

(set-face-attribute 'default nil :height 160)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure mac modifiers to be what you expect, and turn off the bell noise

(setq ring-bell-function 'ignore)
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
(setq indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

(use-package diminish
  :defer nil
  :config
  (diminish 'auto-revert-mode)
  (diminish 'evil-collection-unimpaired-mode)
  (diminish 'eldoc-mode))

(defvar grant/leader-key "SPC")
(defvar grant/local-key ",")
(use-package evil
  :init
  (setq evil-move-beyond-eol t
    evil-normal-state-cursor '(box "white")
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

(use-package evil-multiedit
  :init
  (setq evil-multiedit-follow-matches t)
  :config
  (evil-multiedit-default-keybinds))

(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode 1))

(use-package helm
  :defer f
  :diminish helm-mode
  :init
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
    helm-ff-search-library-in-sexp            t ; search for library in `require' and `declare-function' sexp.
    helm-scroll-amount                        8 ; scroll 8 lines other window using M-<next>/M-<prior>
    helm-ff-file-name-history-use-recentf     t)
  :config
  (helm-mode 1))

(use-package helm-rg)

(use-package helm-swoop
  :init
  (setq helm-swoop-use-fuzzy-match t
    helm-swoop-split-with-multiple-windows t
    helm-swoop-speed-or-color t))

(use-package company
  :diminish
  :config
  (global-company-mode))

(use-package magit)

(use-package projectile
  :init
  (setq projectile-completion-system 'helm
    projectile-project-search-path '("~/repos/" "~/dev/"))
  :config
  (projectile-mode 1))

(use-package helm-projectile)

(use-package flycheck
  :diminish
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  :config
  (global-flycheck-mode))

(use-package lsp-mode
  :init
  (setq read-process-output-max (* 1024 1024)
    lsp-idle-delay 0.500))

(defvar grant/init-file (file-name-directory (or load-file-name (buffer-file-name))))

(load-file (concat grant/init-file "elisp/keys.el"))
(load-file (concat grant/init-file "elisp/python.el"))
(load-file (concat grant/init-file "elisp/go.el"))
(load-file (concat grant/init-file "elisp/clojure.el"))

;; (byte-recompile-directory "~/evil-emacs/straight/build/" 0)
;; (byte-recompile-directory "~/evil-emacs/elisp/")
;; (native-compile-async "~/evil-emacs/straight/build/" 'recursively)

(provide 'init)

;;; init.el ends here
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  '(helm-minibuffer-history-key "M-p")
  '(warning-suppress-types '(((flymake flymake)))))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
  )
