;;; 41-hugsql.el --- HugSQL  -*- lexical-binding: t; -*-
;; HugSQL is a Clojure ORM which adds some special syntax to SQL.
;; This declaration is only here for config encapsulation, it doesn't
;; actually install a package.
;; In this case we install a fix to make imenu work in HugSQL files.
(use-package hugsql
  :straight nil
  :defer t
  :init
  (defun sulami/init-hugsql-imenu ()
    (when (string-suffix-p ".hug.sql" (buffer-file-name))
      (setq
       imenu-generic-expression
       '((nil "^--[[:space:]]:name[[:space:]]+\\([[:alnum:]-]+\\)" 1)))))
  :hook
  (sql-mode . sulami/init-hugsql-imenu))
