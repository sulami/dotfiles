;;; 10-corfu.el --- Corfu  -*- lexical-binding: t; -*-
;; Corfu does completion via a dropdown that automatically pops up
;; while typing. I can select a match if I want to, but ignore the
;; dropdown if I don't.
(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-quit-no-match 'separator)
  :hook (after-init . global-corfu-mode))
