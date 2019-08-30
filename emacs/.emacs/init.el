;; -*- lexical-binding: t; -*-

;; Startup time optimisations stolen from doom-emacs (including the lexical
;; binding bit above)
;; Prevent GC during launch
(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

;; We don't need file handlers during launch either
(defvar sulami--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Don't use package.el
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install & load up use-package
;; This is over here because it shaves a solid second off the startup time if I
;; compile it in.
(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))

;; Eval the actual config
(require 'org-install)
(require 'ob-tangle)
(org-babel-load-file "/Users/sulami/.emacs/README.org")

;; Reset file handlers & GC settings after we're done loading
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq file-name-handler-alist sulami--file-name-handler-alist)
            (setq gc-cons-threshold 16777216
                           gc-cons-percentage 0.1)))
