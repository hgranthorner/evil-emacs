;;; Keys --- my keybindings.
;;; Commentary:
;;; All keybindings for my Emacs set up.
;;; Code:

(use-package general
  :config
  (define-key dired-mode-map (kbd "<normal-state> SPC") nil)

  (general-define-key
   :keymaps 'helm-map
    "<escape>" 'helm-keyboard-quit)

  (general-define-key
    "M-x" 'helm-M-x)

  (general-define-key
    :states 'motion
    :keymaps 'override
    :prefix grant/leader-key
    "SPC" 'helm-M-x

    "b"   '(:ignore t :wk "buffers")
    "bb"  'helm-mini
    "bl"  'evil-switch-to-windows-last-buffer
    "bk"  'kill-buffer
    "bd"  'kill-buffer
    "bB"  'ibuffer

    "c"   '(:ignore t :wk "code")
    "cd"  'lsp-find-definition
    "cr"  'lsp-find-references

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
    "hv"  'describe-variable
    "hg"  'general-describe-keybindings

    "s"   '(:ignore t :wk "search")
    "ss"  'helm-swoop
    "sp"  'helm-projectile-rg

    "o"   'other-window
    "p"   '(:keymap projectile-command-map :wk "projectile")
    "q"   'save-buffers-kill-terminal

    "w"   '(:ignore t :wk "windows")
    "wv"  'split-window-horizontally
    "ws"  'split-window-vertically
    "wm"  'delete-other-windows
    "wd"  'delete-window
    "wk"  'delete-window)

  (general-define-key
    :states 'motion
    :keymaps '(emacs-lisp-mode)
    :prefix grant/local-key
    "e"  '(:ignore t :wk "eval")
    "eb" 'eval-buffer
    "ee" 'eval-last-sexp
    "ed" 'eval-defun

    "p"  'eval-print-last-sexp))

(provide 'keys)
;;; keys.el ends here
