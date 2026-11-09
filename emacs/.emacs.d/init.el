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

;; Set environment variables for SSH agent and macOS native compilation
;; Straight and git need a working agent during startup, so this has to
;; happen before the bootstrap. Probe with `ssh-add -l': a Finder launch
;; sets SSH_AUTH_SOCK to launchd's empty listener, which answers nothing
;; but is present, so neither `getenv' nor `file-exists-p' is evidence.
;; gpg-agent serves the YubiKey keys too, so it wins over the password
;; managers, which only hold the git key.
(let ((saved (getenv "SSH_AUTH_SOCK"))
      (candidates (list (expand-file-name "~/.gnupg/S.gpg-agent.ssh")
                        (expand-file-name "~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock")
                        (expand-file-name "~/Library/Containers/com.bitwarden.desktop/Data/.bitwarden-ssh-agent.sock"))))
  (setenv "SSH_AUTH_SOCK" nil)
  (dolist (sock candidates)
    (when (and (file-exists-p sock)
               (not (getenv "SSH_AUTH_SOCK")))
      (setenv "SSH_AUTH_SOCK" sock)
      (unless (eq 0 (call-process "ssh-add" nil nil nil "-l"))
        (setenv "SSH_AUTH_SOCK" nil))))
  (unless (getenv "SSH_AUTH_SOCK")
    (setenv "SSH_AUTH_SOCK" saved)))

(unless (getenv "MACOSX_DEPLOYMENT_TARGET")
  (setenv "MACOSX_DEPLOYMENT_TARGET" "14.0"))

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
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
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

;; Load f.el, which is needed below to expand the path of the actual
;; config file. `org-babel-load-file' implicitly follows a symlink,
;; but then renders the result next to the actual file, and proceeds
;; to fail to load the file next to the symlink. By fixing the config
;; path to be the actual file, we avoid this confusion.
(use-package f
  :straight t)

;; Eval the actual config
(with-eval-after-load 'org
  (require 'ob-tangle)
  (defconst sulami/emacs-config-file (f-canonical "~/.emacs.d/README.org"))
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
