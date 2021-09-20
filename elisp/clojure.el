;;; clojure.el --- clojure

;;; Commentary:
;;
;;; Code:

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook
    (lambda () (setq-local lsp-enable-indentation nil))
    #'lsp-deferred
    (lambda () (setq-local lisp-indent-offset nil))))

(use-package cider
  :init
  (setq clojure-indent-style 'align-arguments)
  :general
  (:states 'motion
    :keymaps '(clojure-mode-map)
    :prefix grant/local-key
    "'"  'cider-jack-in-clj
    "\"" 'cider-jack-in-cljs

    "e"  '(:ignore t :wk "eval")
    "eb" 'cider-eval-buffer
    "ed" 'cider-eval-defun-at-point
    "ee" 'cider-eval-last-sexp

    "h"  '(:ignore t :wk "help")
    "hd" 'cider-doc

    "i"  '(:ignore t :wk "inspect")
    "ii" 'cider-inspect
    "ir" 'cider-inspect-last-result

    "p"  '(:ignore t :wk "print")
    "pd" 'cider-pprint-eval-defun-at-point
    "pp" 'cider-pprint-eval-last-sexp

    "r"  '(:ignore t :wk "repl")
    "rb" 'cider-switch-to-repl-buffer
    "rn" 'cider-repl-set-ns
    "rq" 'cider-quit)
  (:states 'motion
    :keymaps '(cider-inspector-mode-map)
    "n" 'cider-inspector-next-page
    "p" 'cider-inspector-prev-page)
  :config
  (define-key cider-inspector-mode-map (kbd "SPC") nil)
  (define-key cider-inspector-mode-map (kbd "p") nil)
  (define-key cider-inspector-mode-map (kbd "n") nil))

(provide 'clojure)

;;; clojure.el ends here
