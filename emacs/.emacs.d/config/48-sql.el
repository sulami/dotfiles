;;; 48-sql.el --- SQL  -*- lexical-binding: t; -*-
;; I'm using the builtin SQL-mode, but I want it set to highlight for
;; PostgreSQL specifically.
(use-package sql
  :straight nil
  :custom
  (sql-product 'postgres))
