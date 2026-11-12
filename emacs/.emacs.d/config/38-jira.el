;;; 38-jira.el --- JIRA  -*- lexical-binding: t; -*-
;; Use the builtin bug-reference-mode to automatically convert JIRA
;; ticket references into clickable links. Adapted from monotux.
(use-package bug-reference
  :straight nil
  :config
  (defun sulami/bug-reference-setup ()
    (setq bug-reference-bug-regexp (rx (group
                                        (group
                                         (or "CIRCLE"
                                             "BACKPLANE"
                                             "SRE"
                                             "SERVER"
                                             "INFRA"
                                             "PIPE"
                                             "ORCH"
                                             "PER"
                                             "API"
                                             "TASKS")
                                         "-"
                                         (repeat 3 5 digit))))
          bug-reference-url-format "https://circleci.atlassian.net/browse/%s"))
  :hook
  ((bug-reference-mode . sulami/bug-reference-setup)
   (org-mode . bug-reference-mode)))
