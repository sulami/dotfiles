;;; 18-wgrep.el --- Wgrep  -*- lexical-binding: t; -*-
;; This allows running rgrep and then writing to the result buffer,
;; modifying the files matched in place. Quite useful for sweeping
;; changes.
(use-package wgrep
  :defer t
  :commands (wgrep-change-to-wgrep-mode)
  :config
  (setq wgrep-auto-save-buffer t)
  :general
  (local-leader-def
    :keymaps 'grep-mode-map
    "w" 'wgrep-change-to-wgrep-mode))
