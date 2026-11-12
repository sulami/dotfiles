;;; 11-yasnippet.el --- Yasnippet  -*- lexical-binding: t; -*-
;; Snippets. I have a few custom ones. yasnippet-snippets is a huge
;; bundle of useful snippets for all kinds of modes.
(use-package yasnippet
  :general
  (general-imap
    "C-y" 'yas-insert-snippet)
  (:keymaps 'yas-minor-mode-map
   "<tab>" nil
   "TAB" nil
   "<ret>" nil
   "RET" nil)
  :config
  (yas-reload-all)
  :hook
  ((text-mode . yas-minor-mode)
   (prog-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :defer t
  :after (yasnippet))
