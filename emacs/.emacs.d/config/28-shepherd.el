;;; 28-shepherd.el --- Shepherd  -*- lexical-binding: t; -*-
;; Agent sessions driven over ACP, with a list of what is running and
;; which session is mid-turn.
(use-package shepherd
  :straight nil
  :load-path "~/src/shepherd"
  :commands (shepherd-new
             shepherd-list
             shepherd-send-to-project
             shepherd-resume)
  :general
  (leader-def
    "l n" #'shepherd-new
    "l l" #'shepherd-list))
