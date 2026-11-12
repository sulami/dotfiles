;;; 23-org-mode.el --- Org mode  -*- lexical-binding: t; -*-
;; Org is used for notes and this file's prose, but nothing needs it during
;; startup, so it is only put on `load-path' here and loaded on first use.
;;
;; `:straight t :defer t' (rather than a bare `straight-use-package') keeps it
;; declared for `sulami/update-packages' and straight's freeze files. Removing
;; the declaration entirely would silently fall back to the org bundled with
;; Emacs (9.8.7 instead of the straight build).
(use-package org
  :straight t
  :defer t)

;; Everything below configures org and has to run once org is loaded: these are
;; its defcustoms, and `org-babel-do-load-languages' is autoloaded, so it would
;; otherwise pull org in during startup and defeat the deferral.
(with-eval-after-load 'org

  ;; Add more workflow states
  (setq org-todo-keywords '((sequence "TODO(t)"
                                      "WIP(p)"
                                      "WAITING(w)"
                                      "|"
                                      "DONE(d)"
                                      "CANCELLED(c)")))
  ;; Save when I change a workflow state
  (add-hook 'org-trigger-hook 'save-buffer)
  ;; Enable babel for more languages
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (shell . t)
     (python . t)))
  ;; Use drawers for source block evaluation
  (add-to-list 'org-babel-default-header-args '(:results . "replace drawer"))
  ;; Disable ligatures in org-mode
  (add-hook 'org-mode-hook
            (lambda ()
              (auto-composition-mode -1)))
  ;; Archiving
  ;; - archive into one shared file
  ;; - auto-save
  (setq org-archive-location "~/Documents/Notes/archive.org::"
        org-archive-subtree-add-inherited-tags t)
  ;; Capture
  ;; My capture templates. Also reload buffers before attempting to capture
  ;; to avoid overwriting iCloud changes.
  (setq org-capture-templates
        '(("b" "Blog idea" entry
           (file "blog.org")
           "* %^{title}\n%u\n%?"
           :prepend t)
          ("f" "File link" entry
           (file "inbox.org")
           "* %^{title}\n%a\n%?")
          ("n" "Note" entry
           (file "inbox.org")
           "* %^{title}\n%u\n%?")
          ("t" "Thought" entry
           (file "thoughts.org")
           "* %^{title}\n%u\n%?"))))
