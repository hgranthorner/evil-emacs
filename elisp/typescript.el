;;; typescript.el --- typescript

;;; Commentary:
;;
;;; Code:

(require 'general)

(use-package rjsx-mode
  :defer t
  :hook (rjsx-mode . rainbow-delimiters-mode)
  :config
  (setq js-chain-indent t
	;; These have become standard in the JS community
	js2-basic-offset 2
	;; Don't mishighlight shebang lines
	js2-skip-preprocessor-directives t
	;; let flycheck handle this
	js2-mode-show-parse-errors nil
	js2-mode-show-strict-warnings nil
	;; Flycheck provides these features, so disable them: conflicting with
	;; the eslint settings.
	js2-strict-missing-semi-warning nil
	;; maximum fontification
	js2-highlight-level 3
	js2-idle-timer-delay 0.15))

(define-derived-mode typescript-tsx-mode web-mode "TypeScript-TSX")
(use-package typescript-mode
  :hook ((typescript-mode . rainbow-delimiters-mode)
	 (typescript-tsx-mode . rainbow-delimiters-mode)
	 (typescript-mode . lsp-deferred)
	 (typescript-tsx-mode . lsp-deferred))
  :init
  (add-to-list 'auto-mode-alist
	       (cons "\\.tsx\\'"
		     #'typescript-tsx-mode))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-mode)
  (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
  (flycheck-add-mode 'typescript-tslint 'typescript-tsx-mode))

(use-package web-mode
  :init
  (setq web-mode-markup-indent-offset 2
	web-mode-attr-indent-offset 2
	web-mode-attr-value-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-code-indent-offset 2
	web-mode-block-padding 2
	web-mode-comment-style 2
	web-mode-enable-css-colorization t
	web-mode-enable-auto-pairing t
	web-mode-enable-comment-keywords t
	web-mode-enable-current-element-highlight t
	web-mode-enable-auto-indentation nil
	web-mode-enable-current-column-highlight t
	web-mode-enable-current-element-highlight t)
  :config
  (general-define-key
   :states 'motion
   :keymaps '(web-mode-map)
   :prefix grant/local-key
   "k" #'web-mode-element-kill
   "c" #'web-mode-element-clone))

(use-package emmet-mode
  :hook ((css-mode web-mode rjsx-mode)
	 (emmet-mode . (lambda ()
			 (setq emmet-indent-after-insert nil
			       emmet-indentation 2))))
  :config
  (setq emmet-move-cursor-between-quotes t)
  (general-define-key
   :states '(insert motion)
   :keymaps '(emmet-mode-keymap)
   "TAB" 'emmet-expand-line)
  (general-define-key
   :states '(insert motion)
   :keymaps '(emmet-mode-keymap)
   "TAB" 'emmet-expand-line))

(use-package css-mode)
(use-package rainbow-mode)

(provide 'typescript)

;;; typescript.el ends here
