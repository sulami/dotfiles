;;; 07-undo-fu.el --- Undo-fu  -*- lexical-binding: t; -*-
;; This just provides linear undo/redo. As an added bonus, it also does
;; "undo in region".
(use-package undo-fu
  :defer t
  :custom
  (undo-fu-allow-undo-in-region t)
  (undo-fu-ignore-keyboard-quit t))
