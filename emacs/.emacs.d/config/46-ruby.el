;;; 46-ruby.el --- Ruby  -*- lexical-binding: t; -*-
;; The primary language at $DAYJOB. rspec-mode adds imenu support
;; for RSpec.
(use-package rspec-mode
  :general
  (local-leader-def
    :keymaps 'rspec-mode-map
    "t" #'rspec-verify-single
    "T" #'rspec-verify-all))
