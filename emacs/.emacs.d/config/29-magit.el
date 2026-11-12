;;; 29-magit.el --- Magit  -*- lexical-binding: t; -*-
;; This is probably the single best interface for Git out there.

(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (defun sulami/clone-repo (url)
    "Clone the repo at URL into `sulami/source-directory'"
    (interactive "sURL: ")
    (let* ((repo-name (magit-clone--url-to-name url))
           (target-dir (concat sulami/source-directory "/" repo-name)))
      (magit-clone-regular url target-dir nil)))
  (defun sulami/reset-repo ()
    "Reset the current repo to its default state"
    (interactive)
    (magit-checkout (magit-main-branch))
    (magit-pull-from-upstream nil))
  (defun sulami/magit-changelog ()
    "Generate a git changelog and copy it to the kill ring"
    (interactive)
    (kill-new
     (format "Best reviewed commit-by-commit:\n\n%s"
             (shell-command-to-string
              (format "git log %s..HEAD --reverse --pretty=format:'%s'"
                      (magit-main-branch)
                      "### %h %s%n%n%b")))))
  :general
  (leader-def
    "g b" #'magit-blame-addition
    "g h" #'magit-log
    "g n" #'sulami/reset-repo
    "g s" #'magit-status)
  :hook
  ((shell-mode . with-editor-export-editor)
   (term-mode . with-editor-export-editor)
   (eshell-mode . with-editor-export-editor)
   (git-commit-setup . git-commit-turn-on-flyspell)))
