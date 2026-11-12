;;; 20-highlight-symbol.el --- Highlight symbol  -*- lexical-binding: t; -*-
;; I only enable this every now and then.
(use-package auto-highlight-symbol
  :custom
  (ahs-idle-interval 0.1)
  :general
  (leader-def "t h" 'auto-highlight-symbol-mode))
