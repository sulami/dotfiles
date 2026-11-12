;;; 24-git-link.el --- Git link  -*- lexical-binding: t; -*-
;; This package gets me the link to a git repository or line in a file.
(use-package git-link
  :init
  (defun open-git-link-in-browser ()
    (interactive)
    (let ((git-link-open-in-browser t))
      (git-link "origin" (line-number-at-pos) (line-number-at-pos))))
  (defun open-git-repo-in-browser ()
    (interactive)
    (let ((git-link-open-in-browser t))
      (git-link-homepage "origin")))
  :general
  (leader-def
   "g l" 'git-link
   "g L" 'open-git-link-in-browser
   "g r" 'git-link-homepage
   "g R" 'open-git-repo-in-browser))
