;;; 13-parens.el --- Parentheses  -*- lexical-binding: t; -*-
;; Keeps my parentheses balanced.
;; Electric Pair Mode
;; All modes use electric-pair-mode, which is built into Emacs already,
;; for automatically matching parentheses.
(use-package electric-pair-mode
  :straight nil
  :hook
  ((text-mode . electric-pair-local-mode)
   (prog-mode . electric-pair-local-mode)))
