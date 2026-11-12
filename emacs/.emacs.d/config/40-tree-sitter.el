;;; 40-tree-sitter.el --- Tree-sitter  -*- lexical-binding: t; -*-
;; Tree-sitter is a library for parsing source code, which can be used to
;; generate faster and better syntax highlighting than the traditional
;; regexp-based approach.
(use-package tree-sitter
  :hook
  ((after-init . global-tree-sitter-mode)
   (tree-sitter-after-on . tree-sitter-hl-mode)))

(use-package tree-sitter-langs)
