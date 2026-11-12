;;; 19-hl-todo.el --- Highlight TODO  -*- lexical-binding: t; -*-
;; Highlights certain keywords in comments, like TODO and FIXME.
(use-package hl-todo
  :defer t
  :config
  (add-to-list 'hl-todo-keyword-faces
               `("N\.?B\.?" . ,(cdr (assoc "NOTE" hl-todo-keyword-faces))))
  :hook (after-init . global-hl-todo-mode))
