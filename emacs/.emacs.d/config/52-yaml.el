;;; 52-yaml.el --- YAML  -*- lexical-binding: t; -*-
;; Sometimes I'm a YAML-engineer, too.
(use-package yaml-mode
  :defer t
  :config
  (defun sulami/yaml-setup ()
    (setq evil-shift-width 2))
  :hook
  (yaml-mode . sulami/yaml-setup))
