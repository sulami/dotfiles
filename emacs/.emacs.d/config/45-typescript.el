;;; 45-typescript.el --- Typescript  -*- lexical-binding: t; -*-
(use-package typescript-mode
  :defer t
  :custom
  (typescript-indent-level 2))

(use-package tide
  :defer t
  :config
  (defun sulami/tide-setup ()
    (when (equal "tsx"
                 (file-name-extension buffer-file-name))
      (tide-setup)
      (flycheck-mode +1)
      (eldoc-mode +1)))
  :hook
  ((typescript-mode . sulami/tide-setup)
   (web-mode . sulami/tide-setup)))

(use-package prettier-js
  :defer t
  :hook
  ((typescript-mode . prettier-js-mode)
   (web-mode . prettier-js-mode)))
