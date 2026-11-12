;;; 00-emacs.el --- Better defaults  -*- lexical-binding: t; -*-
(setq user-full-name "Robin Schroer"
      user-login-name "sulami"
      user-mail-address "emacs@sulami.xyz"
      sulami/source-directory "~/src")
;; Discard the custom file
;; custom.el is hidden state, all config is declarative.
(setq custom-file (make-temp-file ""))
;; Hide Backups
;; This way we lose everything backups if the whole machine crashes, but
;; we don't accidentally leave backups around.
(setq backup-directory-alist '(("." . "~/emacs-backup/"))
      auto-save-file-name-transforms '((".*" "~/emacs-auto-save/" t))
      create-lockfiles nil)
;; Revert changed files
;; This reloads changed files which are open in Emacs but not edited.
(setq revert-without-query '(".*"))
;; No trash
(setq delete-by-moving-to-trash nil)
;; Remember recent files
(use-package recentf
  :straight nil
  :custom
  (recentf-max-saved-items 1024)
  :hook
  (after-init . recentf-mode))
;; Be quiet on startup
(setq inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t)
;; Don't tell me about keybindings
(setq suggest-key-bindings nil)
;; Make Tramp great again
;; These are just fixes to make TRAMP work and be reasonably fast. Mostly
;; sourced from the internet, I don't pretend to know how this actually
;; works.
(setq tramp-default-method "ssh"
      tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C'")

;; Various speedups
;; from https://www.gnu.org/software/emacs/manual/html_node/tramp/Frequently-Asked-Questions.html
(setq remote-file-name-inhibit-cache 3600
      tramp-completion-reread-directory-timeout nil
      vc-ignore-dir-regexp (format "%s\\|%s"
                                   vc-ignore-dir-regexp
                                   tramp-file-name-regexp)
      tramp-verbose 0)

;; Disable the history file on remote hosts
(setq tramp-histfile-override t)

;; Save backup files locally
;; from https://stackoverflow.com/a/47021266
(add-to-list 'backup-directory-alist
             (cons tramp-file-name-regexp "/tmp/emacs-backup/"))
;; Remember where we were
(require 'saveplace)
(save-place-mode 1)
;; Always follow symbolic links
(setq vc-follow-symlinks t)
;; Don't ring the bell
(setq ring-bell-function 'ignore)
;; Don't blink the cursor
;; I find this mostly distracting.
(blink-cursor-mode -1)
;; Start the scratch buffer empty
(setq initial-scratch-message "")
;; Enable winner-mode
;; This allows me to use winner-undo if I accidentally delete a window.
(add-hook 'after-init-hook 'winner-mode)
;; Default to Elisp
(setq initial-major-mode 'emacs-lisp-mode)
;; Switch to the help window
(setq help-window-select t)
;; Spaces > tabs
(setq-default indent-tabs-mode nil)
;; Tabs are 4 spaces
(setq-default tab-width 4)
;; Sentences end with a single space
(setq sentence-end-double-space nil)
;; Show trailing whitespace
;; It's disabled by default, and then gets enabled for all file-based
;; buffer modes, so not for REPLS and shells.
(setq-default show-trailing-whitespace nil)
(defun sulami/show-trailing-whitespace ()
  "Just sets `show-trailing-whitespace'."
  (setq show-trailing-whitespace t))
(add-hook 'prog-mode-hook 'sulami/show-trailing-whitespace)
(add-hook 'text-mode-hook 'sulami/show-trailing-whitespace)
;; Show empty lines
;; This shows vim-style tildes on the left fringe.
(when (display-graphic-p)
  (setq-default indicate-empty-lines t)
  (define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
  (setcdr (assq 'empty-line fringe-indicator-alist) 'tilde))
;; Enable so-long-mode
;; This disables expensive modes when a buffer has very long lines to
;; prevent performance issues.
(if (version<= "27.1" emacs-version)
    (global-so-long-mode 1))
;; Highlight matching parentheses
;; I prefer this over using rainbow parentheses, which make it difficult
;; to see what's actually happening.
(show-paren-mode 1)
;; Scrolling
;; These settings were lifted off the internet™ and make scrolling with pointing
;; devices feel more reasonable.
(setq mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
;; Line numbers
;; Show line numbers in every buffer by default. display-line-numbers is
;; the C implementation rather than linum-mode, and it stays a
;; buffer-local variable, so sulami/toggle-line-numbers still turns them
;; off for a single buffer.
(global-display-line-numbers-mode 1)
;; No line wrapping
;; At least as a default, much nicer when resizing windows.
(set-default 'truncate-lines t)
(setq line-move-visual nil)
;; UTF-8
(setq-default buffer-file-coding-system 'utf-8)
(setenv "LANG" "en_US.UTF-8")
(setenv "LC_ALL" "en_US.UTF-8")
(prefer-coding-system 'utf-8)
;; Spelling
;; Use aspell with British English.
(use-package flyspell
  :straight nil
  :custom
  (ispell-program-name "aspell")
  (ispell-extra-args (quote ("--sug-mode=ultra" "--lang=en_GB-ise")))
  (flyspell-sort-corrections nil)
  (flyspell-issue-message-flag nil)
  :hook
  (prog-mode . flyspell-prog-mode))
;; Enable erase buffer
(put 'erase-buffer 'disabled nil)
;; Y/N for yes or no questions
(fset 'yes-or-no-p 'y-or-n-p)
;; Ask before exiting
(setq confirm-kill-emacs 'yes-or-no-p)
;; Frame title
;; Set the frame title to the current project name. This is useful if I
;; have several frames/Emacsen open and want to switch between them.
(setq frame-title-format
      (list :eval '(let ((p-name (projectile-project-name)))
		     (if (string-equal p-name "-")
			 "Emacs"
		       (concat "Emacs - " p-name)))))
;; Disable all the GUI
(if (and (fboundp 'tool-bar-mode)
         tool-bar-mode)
    (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))
;; Setup SSH agent
;; Probe the candidate agents instead of trusting SSH_AUTH_SOCK or the
;; socket files. A Finder launch always leaves SSH_AUTH_SOCK pointing at
;; launchd's empty listener, and a socket file outlives the agent that
;; owned it, so neither tells us whether an agent actually answers.
;; ssh-add -l does: it exits 0 only when an agent responds.
;; gpg-agent comes first because with enable-ssh-support it serves the
;; YubiKey identities in addition to the git key, while 1Password and
;; Bitwarden only hold the git key. Preferring them silently drops the
;; YubiKey keys.
(let ((saved (getenv "SSH_AUTH_SOCK"))
      (candidates (list (expand-file-name "~/.gnupg/S.gpg-agent.ssh")
                        (expand-file-name "~/Library/Group Containers/2BUA8C4S2C.com.1password/t/agent.sock")
                        (expand-file-name "~/Library/Containers/com.bitwarden.desktop/Data/.bitwarden-ssh-agent.sock"))))
  (setenv "SSH_AUTH_SOCK" nil)
  (dolist (sock candidates)
    (when (and (file-exists-p sock)
               (not (getenv "SSH_AUTH_SOCK")))
      (setenv "SSH_AUTH_SOCK" sock)
      (unless (ignore-errors (eq 0 (call-process "ssh-add" nil nil nil "-l")))
        (setenv "SSH_AUTH_SOCK" nil))))
  (unless (getenv "SSH_AUTH_SOCK")
    (setenv "SSH_AUTH_SOCK" saved)))
;; The socket is resolved by path rather than with gpgconf, which is not
;; on the PATH of a Finder launch.
;; Enable recursive minibuffers
;; For example to yank while entering into the minibuffer.
(setq enable-recursive-minibuffers t)
