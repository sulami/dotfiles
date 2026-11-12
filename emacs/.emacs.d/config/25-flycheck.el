;;; 25-flycheck.el --- Flycheck  -*- lexical-binding: t; -*-
;; Flycheck does automatic linting. I enable it mostly manually, as I
;; don't have a lot of linters setup.
(use-package flycheck
  :config
  ;; Disable flycheck on-the-fly-checking if the line count exceeds 2000.
  (setq flycheck-check-syntax-automatically
        (if (> (sulami/buffer-line-count) 2000)
            (delete 'idle-change flycheck-check-syntax-automatically)
          (add-to-list 'flycheck-check-syntax-automatically 'idle-change)))
  :custom
  (flycheck-emacs-lisp-load-path 'inherit)
  :general
  (leader-def "t c" 'flycheck-mode)
  :hook
  ((rust-mode . flycheck-mode)
   (go-mode . flycheck-mode)))
