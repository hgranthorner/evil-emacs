;;; Keys --- my keybindings.
;;; Commentary:
;;; All keybindings for my Emacs set up.
;;; Code:

(use-package general
  :config
  (general-auto-unbind-keys)

  (define-key dired-mode-map (kbd "<normal-state> SPC") nil)

  (general-define-key
   :keymaps 'helm-map
   "<escape>" 'helm-keyboard-quit)

  (general-define-key
   :keymaps 'override
   "M-x"  'helm-M-x
   "<f2>" 'projectile-run-async-shell-command-in-root
   "<f3>" 'projectile-test-project
   "<f5>" 'revert-buffer)

  (general-define-key
   :states 'motion
   :keymaps 'override
   :prefix grant/leader-key

   "."   'repeat
   "k"   'helm-show-kill-ring
   "o"   'other-window
   "SPC" 'helm-M-x
   "q"   'save-buffers-kill-terminal

   "b"   '(:ignore t :wk "buffers")
   "bb"  'helm-mini
   "bl"  'evil-switch-to-windows-last-buffer
   "bk"  'kill-buffer
   "bd"  'kill-buffer
   "bB"  'ibuffer

   "c"   '(:ignore t :wk "code")
   "ca"  'lsp-execute-code-action
   "cd"  'lsp-find-definition
   "cD"  'lsp-find-references
   "ci"  'lsp-organize-imports
   "cr"  'lsp-rename

   "cf"  '(:ignore t :wk "format")
   "cfb" 'lsp-format-buffer
   "cfr" 'lsp-format-region

   "f"   '(:ignore t :wk "files")
   "ff"  'helm-find-files
   "fd"  'dired
   "fs"  'save-buffer

   "g"   '(:ignore t :wk "git")
   "gg"   'magit-status

   "h"   '(:ignore t :wk "help")
   "ha"  'helm-apropos
   "hk"  'describe-key
   "hf"  'describe-function
   "hm"  'describe-mode
   "hv"  'describe-variable
   "hg"  'general-describe-keybindings

   "s"   '(:ignore t :wk "search")
   "si"  'helm-imenu
   "ss"  'helm-swoop
   "sp"  'helm-projectile-rg

   "p"   '(:keymap projectile-command-map :wk "projectile")

   "w"   '(:ignore t :wk "windows")
   "wv"  'split-window-horizontally
   "ws"  'split-window-vertically
   "wm"  'delete-other-windows
   "wd"  'delete-window
   "wh"  'evil-window-left
   "wj"  'evil-window-down
   "wk"  'evil-window-up
   "wl"  'evil-window-right)

  (general-define-key
   :states 'motion
   :keymaps '(emacs-lisp-mode-map lisp-mode-map)
   :prefix grant/local-key

   "s"  '(:ignore t :wk "slurp")
   "sf" 'paredit-forward-slurp-sexp
   "sb" 'paredit-backward-slurp-sexp

   "b"  '(:ignore t :wk "barf")
   "bf" 'paredit-forward-barf-sexp
   "bb" 'paredit-backward-barf-sexp

   "e"  '(:ignore t :wk "eval")
   "eb" 'eval-buffer
   "ee" 'eval-last-sexp
   "ed" 'eval-defun

   "p"  'eval-print-last-sexp))

(provide 'keys)
;;; keys.el ends here
