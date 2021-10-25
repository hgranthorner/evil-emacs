;;; common-lisp.el --- common lisp

;;; Commentary:
;;
;;; Code:
(require 'use-package)
(require 'general)

(use-package sly
  :config
  (general-define-key
   :states 'motion
   :keymaps '(common-lisp-mode-map lisp-mode-map)
   :prefix grant/local-key
   "e" '(:ignore t :wk "eval")
   "eb" 'sly-eval-buffer
   "ee" 'sly-eval-last-expression
   "ed" 'sly-eval-defun

   "'" 'sly))

(provide 'common-lisp)

;;; go.el ends here
