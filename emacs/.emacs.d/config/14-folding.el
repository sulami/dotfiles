;;; 14-folding.el --- Code folding  -*- lexical-binding: t; -*-
(use-package code-folding
  :straight nil
  :config
  (defun sulami/close-defun-fold ()
    "Hide the top-level definition at point."
    (interactive)
    (beginning-of-defun)
    (hs-hide-block))
  :general
  (general-nmap
    "z C" #'sulami/close-defun-fold)
  :hook
  (prog-mode . hs-minor-mode))
