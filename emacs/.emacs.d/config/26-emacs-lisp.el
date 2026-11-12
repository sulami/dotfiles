;;; 26-emacs-lisp.el --- Emacs Lisp  -*- lexical-binding: t; -*-
;; Just some bindings for interacting with Emacs Lisp.
(local-leader-def
  :keymaps 'emacs-lisp-mode-map
  "e" '(:ignore t :wk "eval")
  "e b" 'eval-buffer
  "e e" 'eval-last-sexp
  "e f" 'eval-defun
  "e r" 'eval-region)
