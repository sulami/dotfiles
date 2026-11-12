;;; 12-popper.el --- Popper  -*- lexical-binding: t; -*-
;; Popper deals with all those temporary popup windows.
(use-package popper
  :after (projectile)
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "\\*Async Shell Command\\*"
          help-mode
          helpful-mode
          backtrace-mode
          compilation-mode
          vterm-mode
          rustic-compilation-mode
          rustic-cargo-run-mode
          rustic-cargo-test-mode
          rustic-rustfmt-mode
          rspec-compilation-mode))
  (setq popper-display-control t
        popper-display-function #'display-buffer-use-least-recent-window)
  (popper-mode +1)
  (popper-echo-mode +1)
  :custom
  (popper-group-function #'popper-group-by-projectile)
  :general
  ("s-p" #'popper-toggle-latest
   "s-g" #'popper-cycle))
