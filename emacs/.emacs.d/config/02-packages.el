;;; 02-packages.el --- Package management  -*- lexical-binding: t; -*-
;; use-package
;; Default straight to install anything use-package defines.
(setq straight-use-package-by-default t)
;; el-patch
;; Allows for patching functions in packages.
(use-package el-patch
  :demand t)
;; Dash
;; List library that comes in handy.
(use-package dash)
;; Updating all packages
(defun sulami/update-packages ()
  "Prunes and updates packages, revalidates patches."
  (straight-prune-build-directory)
  (straight-pull-all)
  (el-patch-validate-all)
  (straight-freeze-versions)
  (byte-recompile-directory "~/.emacs.d/straight/build" nil 'force))
