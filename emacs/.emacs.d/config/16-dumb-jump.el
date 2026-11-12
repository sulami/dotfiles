;;; 16-dumb-jump.el --- Dumb jump  -*- lexical-binding: t; -*-
;; Uses rg to jump to definition. Zero setup. Not always correct, but
;; usually good enough. Much less of a hassle than LSP.
(use-package dumb-jump
  :after (evil)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (dumb-jump-selector 'completing-read)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))
