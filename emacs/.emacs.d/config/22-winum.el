;;; 22-winum.el --- Winum  -*- lexical-binding: t; -*-
;; Number windows and allow me to switch to them.
(use-package winum
  :general
  ("s-1" 'winum-select-window-1
   "s-2" 'winum-select-window-2
   "s-3" 'winum-select-window-3
   "s-4" 'winum-select-window-4
   "s-5" 'winum-select-window-5
   "s-6" 'winum-select-window-6
   "s-7" 'winum-select-window-7
   "s-8" 'winum-select-window-8
   "s-9" 'winum-select-window-9)
  (leader-def
    "w 1" 'winum-select-window-1
    "w 2" 'winum-select-window-2
    "w 3" 'winum-select-window-3
    "w 4" 'winum-select-window-4
    "w 5" 'winum-select-window-5
    "w 6" 'winum-select-window-6
    "w 7" 'winum-select-window-7
    "w 8" 'winum-select-window-8
    "w 9" 'winum-select-window-9)
  :hook (after-init . winum-mode))
