;;; 51-markdown.el --- Markdown  -*- lexical-binding: t; -*-
;; I think it's vastly inferior to org-mode, but I still have to use it.
;; Mostly just try to make it look & work like org-mode.
(use-package markdown-mode
  :defer t
  :custom
  (markdown-fontify-code-blocks-natively t)
  :hook
  ((markdown-mode . orgtbl-mode)
   (markdown-mode . flyspell-mode))
  :general
  (local-leader-def
    :keymaps 'markdown-mode-map
    "l" 'markdown-insert-link
    "m" 'markdown-toggle-markup-hiding)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)))

(use-package edit-indirect
  :defer t)
