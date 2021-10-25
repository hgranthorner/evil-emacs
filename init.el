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

(set-face-attribute 'default nil :height 180)

(require 'ansi-color)

(add-hook 'compilation-filter-hook
	  #'(lambda () (ansi-color-apply-on-region compilation-filter-start (point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configure mac modifiers to be what you expect, and turn off the bell noise

(setq ring-bell-function 'ignore)
(defvar is-mac (equal system-type 'darwin))

(use-package exec-path-from-shell)
(if is-mac
    (progn
      (setq ring-bell-function 'ignore
	    mac-command-modifier 'control
	    mac-option-modifier 'meta)
      (exec-path-from-shell-initialize)
      (setq dired-use-ls-dired nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Defaults

(defalias 'yes-or-no-p 'y-or-n-p)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq inhibit-startup-screen t)
(setq backup-directory-alist '(("." . "~/.emacs_backups")))
(setq confirm-kill-emacs 'yes-or-no-p)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(show-paren-mode 1)
(electric-pair-mode 1)
(visual-line-mode 1)
(setq lisp-indent-offset nil)
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq indent-tabs-mode nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom functions
(defun revert-all-no-confirm ()
  "Revert all file buffers, without confirmation.
Buffers visiting files that no longer exist are ignored.
Files that are not readable (including do not exist) are ignored.
Other errors while reverting a buffer are reported only as messages."
  (interactive)
  (let (file)
    (dolist (buf  (buffer-list))
      (setq file  (buffer-file-name buf))
      (when (and file  (file-readable-p file))
        (with-current-buffer buf
          (with-demoted-errors "Error: %S" (revert-buffer t t)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil

(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox t))

(use-package diminish)
(require 'diminish)
(add-hook 'auto-revert-mode-hook (lambda () (diminish 'auto-revert-mode)))
(add-hook 'evil-collection-unimpaired-mode-hook (lambda () (diminish 'evil-collection-unimpaired-mode)))
(add-hook 'eldoc-mode-hook (lambda () (diminish 'eldoc-mode)))

(defvar grant/leader-key "SPC")
(defvar grant/local-key ",")
(use-package evil
  :init
  (setq evil-move-beyond-eol t
	evil-normal-state-cursor '(box "white")
	evil-insert-state-cursor '((bar . 2) "#98BE65")
	evil-visual-state-cursor '(box "orange")
	evil-emacs-state-cursor  '(box "purple")
	evil-undo-system         'undo-redo
	evil-want-keybinding     nil
	evil-respect-visual-line-mode t
	evil-want-Y-yank-to-eol t)
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
  :init
  (setq company-minimum-prefix-length 1)
  :config
  (global-company-mode))

(use-package magit
  :config
  (define-key magit-mode-map (kbd "SPC") nil)
  (define-key magit-diff-section-base-map (kbd "SPC") nil))

(use-package paredit)

(use-package projectile
  :init
  (require 'uniquify)
  (setq projectile-completion-system 'helm
	projectile-project-search-path '("~/dev/" "~/repos/")
	uniquify-buffer-name-style 'forward)
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

(use-package yaml-mode)

(use-package darkroom)

(defvar grant/init-file
  (file-name-directory
   (or load-file-name (buffer-file-name))))

(load-file (concat grant/init-file "elisp/keys.el"))
(load-file (concat grant/init-file "elisp/python.el"))
(load-file (concat grant/init-file "elisp/go.el"))
(load-file (concat grant/init-file "elisp/clojure.el"))
(load-file (concat grant/init-file "elisp/typescript.el"))
(load-file (concat grant/init-file "elisp/common-lisp.el"))

;; (byte-recompile-directory "~/evil-emacs/straight/build/" 0)
(byte-recompile-directory "~/evil-emacs/elisp/" 0 1)
(native-compile-async "~/evil-emacs/elisp/" 'recursively)

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
