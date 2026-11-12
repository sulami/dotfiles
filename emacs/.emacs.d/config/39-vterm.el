;;; 39-vterm.el --- vterm  -*- lexical-binding: t; -*-
;; vterm is a terminal emulator which is way faster than anything built
;; into Emacs. It copes well with huge amounts of output and weird escape
;; sequences. Prior to installation run:
;; Original sh block:
;;     brew install cmake libtool
(use-package vterm
  :defer t
  :commands (vterm)
  :general
  (leader-def
    "a t" 'vterm))
