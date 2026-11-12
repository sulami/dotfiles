;;; 31-dired.el --- Dired  -*- lexical-binding: t; -*-
;; Dired+
;; This brings some nice quality of life extensions to dired.
(use-package dired+
  :custom
  (diredp-hide-details-initially-flag nil))
;; Enable find-alternate-file
;; This causes RET to open the target in the current buffer, instead of
;; a new buffer. As a result, when traversing directories I don't get one
;; buffer per step, which quickly gets annoying.
;; At some point I need to rebind ^ as well.
(put 'dired-find-alternate-file 'disabled nil)
;; Always show me current data
(add-hook 'dired-mode-hook 'auto-revert-mode)
;; Use human-readable formats
(setq-default dired-listing-switches "-alh")
;; Copy recursively by default
(setq dired-recursive-copies 'always)
