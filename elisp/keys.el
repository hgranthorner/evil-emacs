;;; Keys --- my keybindings.
;;; Commentary:
;;; All keybindings for my Emacs set up.
;;; Code:

(defmacro grant/create-normal-mode-map (map-sym prefix &rest keys)
  "Generate the requisite keymaps and bindings for keybindings in normal mode.
MAP-SYM - an unquoted symbol that will be the basis of the name of the keymap.
PREFIX  - a string that is the prefix for the rest of the bindings.
KEYS    - an associative list of strings (the key) to the function (the bindings)."
  (let* ((m (symbol-name map-sym))
          (map-name (concat m "-normal-map"))
          (definer (intern (concat m "-normal-prefix"))))
    `(progn
       (defvar ,(intern map-name) (make-sparse-keymap))
       (general-create-definer ,definer :prefix ,prefix)
       (,definer :keymaps 'normal
         "" nil
         ,@keys))))


(use-package general
  :config
  (general-define-key
    :keymaps 'helm-map
    "<escape>" 'helm-keyboard-quit)

  (grant/create-normal-mode-map local
    ","
    "p" 'eval-print-last-sexp)

  (grant/create-normal-mode-map eval
    ", e"
    "e" 'eval-last-sexp
    "d" 'eval-defun)

  (grant/create-normal-mode-map leader
    "SPC"
    "SPC" 'helm-M-x
    "b"   '(:ignore t :wk "buffers")
    "f"   '(:ignore t :wk "files")
    "g"   'magit-status
    "h"   '(:ignore t :wk "help")
    "s"   'helm-swoop
    "h"   'helm-apropos
    "o"   'other-window
    "p"   '(:keymap projectile-command-map :wk "projectile")
    "q"   'save-buffers-kill-terminal
    "w"   '(:ignore t :wk "windows"))

  (grant/create-normal-mode-map help
    "SPC h"
    "a" 'helm-apropos
    "k" 'describe-key
    "f" 'describe-function
    "v" 'describe-variable
    "g" 'general-describe-keybindings)

  (grant/create-normal-mode-map buffer
    "SPC b"
    "b" 'helm-mini
    "k" 'kill-buffer
    "d" 'kill-buffer
    "B" 'ibuffer)

  (grant/create-normal-mode-map file
    "SPC f"
    "f" 'helm-find-files
    "d" 'dired
    "s" 'save-buffer)

  (grant/create-normal-mode-map window
    "SPC w"
    "v" 'split-window-horizontally
    "h" 'split-window-vertically
    "m" 'delete-other-windows
    "d" 'delete-window
    "k" 'delete-window))

(provide 'keys)
;;; keys.el ends here
