;;; 43-go.el --- Go  -*- lexical-binding: t; -*-
;; Every now and then I do have to look at some Go code, sadly.
(use-package go-mode
  :defer t
  :custom
  (gofmt-show-errors 'echo)
  :hook
  (before-save . gofmt-before-save))
