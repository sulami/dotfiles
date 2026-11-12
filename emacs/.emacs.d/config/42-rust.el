;;; 42-rust.el --- Rust  -*- lexical-binding: t; -*-
;; I don't actually really use Rust beyond experiments, so not much
;; config here.
(use-package rustic
  :defer t
  :after (rust-mode flycheck)
  :init
  (add-to-list 'flycheck-checkers 'rustic-clippy)
  :config
  (setq rustic-format-on-save t)
  (setq rustic-lsp-client 'eglot)
  (setq rustic-rustfmt-args "--edition 2021")
  :general
  (local-leader-def
    :keymaps 'rustic-mode-map
    "c" #'rustic-compile
    "r" #'rustic-cargo-run
    "t" #'rustic-cargo-test-dwim
    "T" #'rustic-cargo-run-nextest))

(use-package flycheck-rust
  :defer t
  :after (flycheck)
  :hook
  (flycheck-mode . flycheck-rust-setup))
