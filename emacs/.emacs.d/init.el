;; -*- lexical-binding: t; -*-

;; NOTE
;; This config is a shim that is loading README.org. Everything here
;; is necessary setup to get the tangling and loading working, and
;; also minimise startup speed. This currently starts up on my machine
;; in about 0.3s.

(defconst emacs-start-time (current-time))

;; Startup time optimisations stolen from doom-emacs (including the lexical
;; binding bit above)
;; Prevent GC during launch
(setq gc-cons-threshold 500000000
      gc-cons-percentage 0.6)

;; We don't need file handlers during launch either
(defvar sulami--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Don't use package.el
(setq package-enable-at-startup nil
      package--init-file-ensured t)

;; Don't check packages for rebuild on startup
(setq straight-check-for-modifications '(find-when-checking
                                         check-on-save))

;; Prefer newer bytecode
(setq load-prefer-newer t)

;; Enable debug-on-error during launch
(setq debug-on-error t)

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

;; Load up org-mode early on to get a current upstream instead of the
;; packaged version.
(use-package org
  :straight t)

;; Eval the actual config
(with-eval-after-load 'org
  (require 'org-install)
  (require 'ob-tangle)
  (defconst sulami/emacs-config-file "~/.emacs.d/README.org")
  (org-babel-load-file sulami/emacs-config-file))

;; Reset file handlers & GC settings after we're done loading.
(add-hook 'emacs-startup-hook
          (lambda ()
            (run-with-timer 5
                            nil
                            (lambda ()
                              (setq file-name-handler-alist sulami--file-name-handler-alist)
                              (setq gc-cons-threshold 16777216
                                    gc-cons-percentage 0.1)
                              (setq debug-on-error nil)))))

;; Startup done, print some statistics.
(let ((pkg-count (length (hash-table-keys straight--success-cache)))
      (startup-time (float-time (time-subtract (current-time) emacs-start-time))))
  (message (format "Startup complete, loaded %d packages in %.2fs"
                   pkg-count
                   startup-time)))
