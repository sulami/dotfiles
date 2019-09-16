;; -*- lexical-binding: t; -*-

(setq custom-file (make-temp-file ""))

(setq backup-directory-alist '(("." . "/tmp/emacs-backup")))

(setq delete-by-moving-to-trash nil)

(when (require 'recentf)
  (recentf-mode 1)
  (setq recentf-max-saved-items 255))

(setq inhibit-splash-screen t
      inhibit-startup-screen t
      inhibit-startup-message t)

(setq suggest-key-bindings nil)

(setq tramp-ssh-controlmaster-options
      "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o Control-Persist=no")

(require 'saveplace)
(save-place-mode 1)

(setq ring-bell-function 'ignore)

(setq initial-scratch-message "")

(setq initial-major-mode 'emacs-lisp-mode)

(setq help-window-select t)

(setq-default indent-tabs-mode nil)

(setq-default show-trailing-whitespace t)

(setq-default indicate-empty-lines t)
(define-fringe-bitmap 'tilde [0 0 0 113 219 142 0 0] nil nil 'center)
(setcdr (assq 'empty-line fringe-indicator-alist) 'tilde)

(show-paren-mode 1)

(setq mouse-wheel-progressive-speed nil
      mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))

(set-default 'truncate-lines t)
(setq line-move-visual nil)

(setq-default buffer-file-coding-system 'utf-8)
(setenv "LANG" "en_be.UTF-8")
(prefer-coding-system 'utf-8)

(setq ispell-program-name "aspell"
      ispell-extra-args (quote ("--sug-mode=ultra" "--lang=en_GB-ise")))

(fset 'yes-or-no-p 'y-or-n-p)

(setq confirm-kill-emacs 'yes-or-no-p)

(setq frame-title-format
      (list :eval '(let ((p-name (projectile-project-name)))
		     (if (string-equal p-name "-")
			 "Emacs"
		       (concat "Emacs - " p-name)))))

(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tooltip-mode) (tooltip-mode -1))

(setq mac-command-modifier 'super
      mac-option-modifier 'meta)

(define-key global-map (kbd "s-v") 'yank)

(define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen)

(define-key global-map (kbd "s-t") 'mac-font-panel-mode)

(setq straight-use-package-by-default t)

(use-package el-patch)

(use-package dash)

(defun sulami/update-packages ()
  "Prunes and updates packages, revalidates patches."
  (straight-prune-build-directory)
  (straight-fetch-all)
  (straight-pull-all)
  (el-patch-validate-all))

(let ((font "Fira Code 14"))
  (set-face-attribute 'default nil :font font)
  (set-frame-font font nil t))
(mac-auto-operator-composition-mode)

(use-package doom-themes
  :after (dash)
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  ;; Patch out the backgound colours from org-mode src blocks
  (set-variable
   'doom-themes-base-faces
   (->> doom-themes-base-faces
        (cl-remove-if (lambda (x) (equal (car x) 'org-block)))
        (cl-remove-if (lambda (x) (equal (car x) 'org-block-background)))
        (cl-substitute-if '(org-block-begin-line :foreground comments)
                          (lambda (x) (equal (car x) 'org-block-begin-line)))))
  (doom-themes-org-config)
  ;; Set the default colourscheme according to the time of day
  :hook (after-init . (lambda ()
                        (let ((hour-of-day (read (format-time-string "%H"))))
                          (if (< 8 hour-of-day 18)
                              (load-theme 'doom-solarized-light t)
                            (load-theme 'doom-one t))))))

(use-package all-the-icons
  :defer t)

(use-package all-the-icons-dired
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package doom-modeline
 :hook (after-init . doom-modeline-mode)
 :config
 (setq doom-modeline-buffer-file-name-style 'relative-to-project
       doom-modeline-buffer-encoding nil
       doom-modeline-persp-name nil
       doom-modeline-vcs-max-length 36))

(setq org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

(add-hook 'org-mode-hook
	  (lambda ()
	    (mac-auto-operator-composition-mode -1)))

(setq org-hide-emphasis-markers nil)

(setq org-indent-indentation-per-level 1)
(add-hook 'org-mode-hook 'org-indent-mode)

;(add-hook 'org-mode-hook 'flyspell-mode)

(setq org-archive-location "archive.org::")

(setq org-directory "~/Documents/Notes/")
(setq org-agenda-files (list org-directory
                             "~/.emacs/README.org"))

(setq org-capture-templates
      '(("t" "Todo" entry
         (file "todo.org")
         "* TODO %?")
        ("f" "File" entry
         (file "todo.org")
         "* TODO %?\n%a")
        ("c" "Climbing journal" entry
         (file "climbing.org")
         "* %t\n%?"
         :prepend t)))

(setq org-journal-dir "~/Documents/org-journal/")

(setq calendar-week-start-day 1)

(use-package org-chef
  :disabled)

(use-package org-jira
  :disabled)

(defun sulami/open-emacs-config ()
  "Opens the config file for our favourite OS."
  (interactive)
  (find-file sulami/emacs-config-file))

(defun sulami/reload-emacs-config ()
  "Loads the config file for our favourite OS."
  (interactive)
  (org-babel-load-file sulami/emacs-config-file))

(defun sulami/rename-file-and-buffer ()
  "Rename the current buffer and file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (message "Buffer is not visiting a file!")
      (let ((new-name (read-file-name "New name: " filename)))
        (cond
         ((vc-backend filename) (vc-rename-file filename new-name))
         (t
          (rename-file filename new-name t)
          (set-visited-file-name new-name t t)))))))

(defun sulami/open-scratch-buffer ()
  "Open the scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun sulami/open-message-buffer ()
  "Open the message buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun sulami/buffer-line-count ()
  "Get the number of lines in the active buffer."
  (count-lines 1 (point-max)))

(defun sulami/save-to-junk ()
  "Save the current buffer as a junk file."
  (interactive)
  (spacemacs/copy-whole-buffer-to-clipboard)
  (kill-buffer)
  (open-junk-file)
  (insert (current-kill 0))
  (save-buffer))

(defun sulami/delete-file-and-buffer ()
  "Deletes a buffer and the file it's visiting."
  (interactive)
  (when-let* ((file-name (buffer-file-name))
              (really (yes-or-no-p (format "Delete %s? "
                                           file-name))))
    (delete-file file-name)
    (kill-buffer)))

(defun sulami/toggle-maximise-window ()
  "Toggles maximising the current window."
  (interactive)
  (let ((el-reg ?F))
    (if (< winum--window-count 2)
        (jump-to-register el-reg)
      (progn
        (window-configuration-to-register el-reg)
        (delete-other-windows)))))

(defun sulami/layout-triple-fib ()
  "Open one window on the left and stacked on the right."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (select-window (next-window))
  (split-window-vertically))

(defun sulami/shell-command-on-region (beg end)
  (interactive "r")
  (if (use-region-p)
      (let ((cmd (read-shell-command "Command: ")))
        (call-process-region beg end cmd t t))
    (message "Select a region first")))

(defun sulami/sprunge-buffer ()
  "Sends the current buffer content to sprunge.us and copy the URL to the
clipboard."
  (interactive)
  (call-process-region
   (point-min) (point-max)
   "curl -sF 'sprunge=<-' http://sprunge.us"
   "*Sprunge*")
  (with-current-buffer "*Sprunge*"
    (message (concat "Sprunged to " (buffer-string)))
    ;(spacemacs/copy-whole-buffer-to-clipboard)
))

(use-package general
  :config
  (general-auto-unbind-keys)
  (general-evil-setup)
  (defconst leader-key "SPC")
  (general-create-definer leader-def
    :prefix leader-key
    :states '(normal visual))
  (defconst local-leader-key ",")
  (general-create-definer local-leader-def
    :prefix local-leader-key
    :states '(normal visual))
  (leader-def
    :keymaps 'override
    ;; Prefixes
    "b" '(:ignore t :wk "buffer")
    "f" '(:ignore t :wk "file")
    "f e" '(:ignore t :wk "emacs")
    "g" '(:ignore t :wk "git")
    "h" '(:ignore t :wk "help")
    "j" '(:ignore t :wk "jump")
    "k" '(:ignore t :wk "lisp")
    "l" '(:ignore t :wk "lsp")
    "p" '(:ignore t :wk "project/perspective")
    "s" '(:ignore t :wk "search")
    "t" '(:ignore t :wk "toggle")
    "w" '(:ignore t :wk "window")
    ;; General keybinds
    "$" 'eshell
    "\\" 'indent-region
    "|" 'sulami/shell-command-on-region
    "b e" 'erase-buffer
    "b d" 'kill-this-buffer
    "b m" 'sulami/open-message-buffer
    "b r" 'sulami/rename-file-and-buffer
    "b s" 'sulami/open-scratch-buffer
    "f e e" 'sulami/open-emacs-config
    "f e r" 'sulami/reload-emacs-config
    "f d" 'dired
    "f D" 'sulami/delete-file-and-buffer
    "f R" 'sulami/rename-file-and-buffer
    "h d" 'describe-symbol
    "h f" 'describe-function
    "h g" 'general-describe-keybindings
    "h k" 'describe-key
    "h l" 'view-lossage
    "h v" 'describe-variable
    "t l" 'toggle-truncate-lines
    "t s" 'flyspell-mode
    "t n" 'linum-mode
    "w =" 'balance-windows
    "w m" 'sulami/toggle-maximise-window)
  (general-define-key
   "s-=" 'text-scale-adjust)
  ;; Org mode
  (local-leader-def
    :keymaps 'org-mode-map
    "a" 'org-archive-subtree)
  ;; Dired
  (general-define-key
   :keymaps 'dired-mode-map
   "<return>" 'dired-find-alternate-file))

(use-package hydra
  :defer t)

(use-package evil
  :after (general)
  :init
  (setq evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-want-Y-yank-to-eol t
        evil-want-keybinding nil)
  :config
  ;; This conflicts with the local leader
  (unbind-key "," evil-motion-state-map)

  (defun sulami/evil-set-jump-wrapper (cmd)
    "Wraps a general command to call `evil-set-jump' before."
    (let ((cmd-name (symbol-name cmd)))
      `((lambda (&rest rest)
          (interactive)
          (evil-set-jump)
          (apply (quote ,cmd) rest))
        :wk ,cmd-name)))
  :general
  (leader-def
   "<tab>" 'evil-switch-to-windows-last-buffer
   "w d" 'evil-window-delete
   "w h" 'evil-window-move-far-left
   "w j" 'evil-window-move-very-bottom
   "w k" 'evil-window-move-very-top
   "w l" 'evil-window-move-far-right
   "w /" 'evil-window-vsplit
   "w -" 'evil-window-split)
  :hook (after-init . evil-mode))

(use-package evil-collection
  :after (evil)
  :config (evil-collection-init))

(use-package evil-search-highlight-persist
  :config
  (defun sulami/isearch-nohighlight ()
    "Remove search highlights if not in the isearch minor mode."
    (interactive)
    (when (not isearch-mode)
      (evil-search-highlight-persist-remove-all)))
  :general
  (general-nmap "RET" 'sulami/isearch-nohighlight)
  :hook (evil-mode . global-evil-search-highlight-persist))

(use-package evil-commentary
  :hook (evil-mode . evil-commentary-mode))

(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))

(use-package which-key
  :hook (after-init . which-key-mode))

(use-package ivy
  :init
  (setq ivy-on-del-error-function #'ignore
        ivy-count-format "(%d/%d) "
        ivy-re-builders-alist '((counsel-projectile-find-file . ivy--regex-fuzzy)
                                (counsel-apropos . ivy--regex-ignore-order)
                                (t . ivy--regex-plus)))
  :config
  (defun sulami/ivy-with-thing-at-point (cmd)
    "Runs an ivy command with the thing at point."
    (let ((ivy-initial-inputs-alist
           (list
            (cons cmd (thing-at-point 'symbol)))))
      (funcall cmd)))
  :general
  (:keymaps 'ivy-minibuffer-map
   "C-w" 'ivy-backward-kill-word)
  :hook (after-init . ivy-mode))

(use-package counsel
  :after (evil)
  :config/el-patch
  ;; Patching counsel-apropos to skip the apropos step
  (defun counsel-apropos ()
  "Show all matching symbols.
See `apropos' for further information on what is considered
a symbol and how to search for them."
  (interactive)
  (ivy-read "Search for symbol (word list or regexp): " obarray
            :predicate (lambda (sym)
                         (or (fboundp sym)
                             (boundp sym)
                             (facep sym)
                             (symbol-plist sym)))
            :history 'counsel-apropos-history
            :preselect (ivy-thing-at-point)
            :sort t
            :action
            (el-patch-swap
              ;; Original
              (lambda (pattern)
                (when (string= pattern "")
                  (user-error "Please specify a pattern"))
                ;; If the user selected a candidate form the list, we use
                ;; a pattern which matches only the selected symbol.
                (if (memq this-command '(ivy-immediate-done ivy-alt-done))
                    ;; Regexp pattern are passed verbatim, other input is
                    ;; split into words.
                    (if (string= (regexp-quote pattern) pattern)
                        (apropos (split-string pattern "[ \t]+" t))
                      (apropos pattern))
                  (apropos (concat "\\`" pattern "\\'"))))
              ;; Patch
              (lambda (sym-name)
                (describe-symbol (intern-soft sym-name))))
            :caller 'counsel-apropos))
  :init
  (defun sulami/imenu-goto-function (NAME POSITION &rest REST)
    "Imenu goto function which pushes an evil jump position before
    jumping."
    (evil-set-jump)
    (apply #'imenu-default-goto-function NAME POSITION REST))
  (setq-default imenu-default-goto-function 'sulami/imenu-goto-function)
  :general
  (leader-def
   ":" 'counsel-M-x
   "b b" 'counsel-switch-buffer
   "f f" 'counsel-find-file
   "f r" 'counsel-recentf
   "h a" 'counsel-apropos
   "j i" 'counsel-semantic-or-imenu)
  (leader-def
    :keymaps 'org-mode-map
    ;; Override imenu for org-mode
    "j i" 'counsel-org-goto)
  :hook (after-init . counsel-mode))

(use-package swiper
  :config
  (defun sulami/swiper-thing-at-point ()
    (interactive)
    (sulami/ivy-with-thing-at-point 'swiper))
  :general
  (leader-def
   "s s" 'swiper
   "s S" 'sulami/swiper-thing-at-point))

(use-package ivy-prescient
  :hook (ivy-mode . ivy-prescient-mode)
  :config
  (prescient-persist-mode))

(use-package ivy-xref
  :defer t
  :init (if (< emacs-major-version 27)
            (setq xref-show-xrefs-function #'ivy-xref-show-xrefs)
          (setq xref-show-definitions-function #'ivy-xref-show-defs)))

(use-package flyspell-correct-ivy
  :defer t
  :init
  (setq flyspell-correct-interface #'flyspell-correct-ivy)
  :general
  (leader-def
    "s c" 'flyspell-correct-wrapper
    ;; FIXME This doesn't work yet
    "s C" '((lambda ()
              (interactive)
              (let ((current-prefix-arg 4))
                (call-interactively 'flyspell-correct-wrapper)))
            :wk "flyspell-correct-wrapper-rapid")))

(use-package flx
  :defer t)

(use-package company
  :init
  (setq company-idle-delay .01)
  :general
  (general-imap
   "C-n" 'company-complete
   "C-p" nil)
  (:keymaps 'company-active-map
   "C-n" 'company-select-next
   "C-p" 'company-select-previous
   "<S-tab>" nil
   "<tab>" 'company-complete-selection
   "C-w" 'evil-delete-backward-word)
  :hook (after-init . global-company-mode))

(use-package company-prescient
  :hook (company-mode . company-prescient-mode))

(use-package yasnippet
  :config
  (setq yas-snippet-dirs (add-to-list #'yas-snippet-dirs "/Users/sulami/.emacs/snippets/"))
  :general
  (:keymaps 'yas-minor-mode-map
   "<tab>" nil
   "TAB" nil
   "<ret>" nil
   "RET" nil)
  :hook (after-init . yas-global-mode))

(use-package ivy-yasnippet
  :general
  (general-imap "C-y" 'ivy-yasnippet))

(use-package yasnippet-snippets
  :defer t
  :after (yasnippet))

(use-package smartparens
  :after (hydra)
  :config
  (require 'smartparens-config)
  (defhydra hydra-wrap (:color blue)
    "wrap"
    ("(" sp-wrap-round)
    ("[" sp-wrap-square)
    ("{" sp-wrap-curly))
  (defhydra hydra-lisp ()
    "lisp"
    ("s" sp-forward-slurp-sexp "slurp")
    ("S" sp-backward-slurp-sexp "slurp backwards")
    ("b" sp-forward-barf-sexp "barf")
    ("B" sp-backward-barf-sexp "barf backwards")
    ("w" hydra-wrap/body "wrap" :color blue)
    ("." nil "quit" :color blue))
  :general
  (leader-def "k" 'hydra-lisp/body)
  :hook (prog-mode . smartparens-global-mode))

(use-package evil-cleverparens
  :init
  (setq evil-cleverparens-use-additional-movement-keys nil
        evil-cleverparens-use-additional-bindings nil
        evil-cleverparens-use-regular-insert t)
  ;; Prevent evil-cleverparens from converting >/< to slurp/barf
  (defun sulami/evil-cp-modify-regular-bindings (&rest r)
    (setq evil-cp-regular-bindings
          (remove-if (lambda (key-string)
                       (member key-string '("_" ">" "<")))
                     evil-cp-regular-bindings
                     :key 'car)))
  (advice-add 'evil-cp--enable-regular-bindings :before
              #'sulami/evil-cp-modify-regular-bindings)
  :hook (prog-mode . evil-cleverparens-mode))

(use-package dumb-jump
  :after (evil)
  :config
  (setq dumb-jump-selector 'ivy
        dumb-jump-force-searcher 'rg)
  :general
  (leader-def
    "j j" (sulami/evil-set-jump-wrapper 'dumb-jump-go)
    "j p" (sulami/evil-set-jump-wrapper 'dumb-jump-go-prompt)))

(use-package avy
  :general
  (general-nmap "s-n" 'avy-goto-word-or-subword-1))

(use-package hl-todo
  :defer t
  :hook (after-init . global-hl-todo-mode))

(use-package auto-highlight-symbol
  :general
  (leader-def "t h" 'auto-highlight-symbol-mode))

(use-package projectile
  :init
  (setq projectile-completion-system 'ivy)
  :config
  (defun sulami/projectile-replace ()
    "Search and replace in the whole project."
    (interactive)
    (dired (projectile-project-root) "-alR")
    (let ((file-regex (read-string "Select files with regex: "))
          (from (read-string "Search for: "))
          (to (read-string "Replace with: ")))
      (dired-mark-files-regexp file-regex)
      (dired-do-find-regexp-and-replace from to))
    (projectile-save-project-buffers)
    (with-current-buffer "*xref*"
      (kill-buffer-and-window))
    ; last open file
    (delete-window)
    ; cleanup dired
    (dired-unmark-all-marks)
    (kill-buffer))
  :general
  (leader-def
    "p r" 'sulami/projectile-replace
    "p d" 'projectile-dired)
  :hook (after-init . projectile-global-mode))

(use-package counsel-projectile
  :defer t
  :config
  (defun sulami/projectile-rg-thing-at-point ()
    (interactive)
    (let ((counsel-projectile-rg-initial-input (thing-at-point 'symbol)))
      (counsel-projectile-rg)))
  :general
  (leader-def
   "p b" 'counsel-projectile-switch-to-buffer
   "p f" 'counsel-projectile-find-file
   "s p" 'counsel-projectile-rg
   "s P" 'sulami/projectile-rg-thing-at-point))

;; (defun sulami/project-root-shell ()
;;   "Pop the default shell in the project root if inside a project, otherwise in
;; the default directory."
;;   (interactive)
;;   (when (sulami/is-spacemacs)
;;     (if (projectile-project-p)
;;         (projectile-with-default-dir (projectile-project-root)
;;           (spacemacs/default-pop-shell))
;;       (spacemacs/default-pop-shell))))

(use-package perspective
  :config
  (setq persp-show-modestring nil)
  :general
  (leader-def
    "p l" 'persp-switch)
  :hook (after-init . persp-mode))

(use-package persp-projectile
  :defer t
  :after (perspective)
  :init
  (defun sulami/kill-project-perspective ()
    "Kills the current project and then the perspective"
    (interactive)
    (projectile-kill-buffers)
    (persp-kill (persp-name (persp-curr))))
  :general
  (leader-def
    "p p" 'projectile-persp-switch-project
    "p k" 'sulami/kill-project-perspective))

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
   "s-9" 'winum-select-window-9
   "s-0" 'winum-select-window-0-or-10)
  :hook (after-init . winum-mode))

(use-package fill-column-indicator
  :general
  (leader-def "t i" 'fci-mode))

(use-package focus
  :general
  (leader-def "t f" 'focus-mode))

(use-package darkroom
  :general
  (leader-def "t d" 'darkroom-tentative-mode))

(use-package magit
  :custom
  (magit-display-buffer-function 'magit-display-buffer-same-window-except-diff-v1)
  :general
  (leader-def
    "g b" 'magit-blame-addition
    "g s" 'magit-status)
  :init
  (setq magit-completing-read-function 'ivy-completing-read))

(use-package evil-magit
  :defer t
  :hook (magit-mode . (lambda () (require 'evil-magit))))

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

(use-package flycheck
  :config
  ;; Disable flycheck on-the-fly-checking if the line count exceeds 2000.
  (setq flycheck-check-syntax-automatically
        (if (> (sulami/buffer-line-count) 2000)
            (delete 'idle-change flycheck-check-syntax-automatically)
          (add-to-list 'flycheck-check-syntax-automatically 'idle-change)))
  :general
  (leader-def "t c" 'flycheck-mode)
  :hook (clojure-mode . flycheck-mode))

(local-leader-def
  :keymaps 'emacs-lisp-mode-map
  "e" '(:ignore t :wk "eval")
  "e b" 'eval-buffer
  "e e" 'eval-sexp
  "e f" 'eval-defun
  "e r" 'eval-region)

(setq eshell-alias-file "~/.emacs/aliases")

(put 'dired-find-alternate-file 'disabled nil)

(use-package esup
  :disabled)

(use-package restclient
  :mode (("\\.http\\'" . restclient-mode))
  :general
  (local-leader-def
    :keymaps 'restclient-mode-map
    "c" 'restclient-copy-curl-command
    "r" 'restclient-http-send-current-raw
    "s" 'restclient-http-send-current-stay-in-window
    "S" 'restclient-http-send-current))

(use-package atomic-chrome
  :init
  (setq atomic-chrome-default-major-mode 'markdown-mode
        atomic-chrome-buffer-open-style 'frame)
  :hook (after-init . atomic-chrome-start-server))

(use-package lsp-mode
  :disabled
  :defer t
  :commands lsp
  :config
  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure-mode"))
  :init
  (setq lsp-enable-indentation nil)
  :general
  (leader-def
    "l f" 'lsp-format-region
    "l F" 'lsp-format-buffer
    "l j" 'lsp-goto-implementation
    "l q" 'lsp-shutdown-workspace
    "l r" 'lsp-rename
    "l R" 'lsp-restart-workspace
    "l u" 'lsp-find-references))

(use-package company-lsp
  :disabled
  :defer t
  :commands company-lsp)

(use-package lsp-ui
  :disabled
  :defer t
  :commands lsp-ui-mode)

(use-package clojure-mode
  :defer t
  :requires (flycheck-clj-kondo)
  :config
  (require 'flycheck-clj-kondo))

(use-package cider
  :defer t
  :hook (clojure-mode . cider-mode)
  :init
  (setq cider-auto-mode nil)
  :general
  (local-leader-def
    :keymaps 'clojure-mode-map
    "c" 'cider-connect
    "j" 'cider-jack-in
    "q" 'cider-quit
    "s" 'cider-scratch
    "x" 'cider-ns-reload-all
    "e" '(:ignore t :wk "eval")
    "e b" 'cider-eval-buffer
    "e d" 'cider-debug-defun-at-point
    "e e" 'cider-eval-sexp
    "e f" 'cider-eval-defun-at-point
    "e r" 'cider-eval-region
    "h" '(:ignore t :wk "help")
    "h a" 'cider-apropos
    "h d" 'cider-doc
    "h i" 'cider-inspect-last-result
    "h w" 'cider-docview-clojuredocs-web
    "r" '(:ignore t :wk "repl")
    "r f" 'cider-insert-defun-in-repl
    "r n" 'cider-insert-ns-form-in-repl
    "r r" 'cider-switch-to-repl-buffer
    "t" '(:ignore t :wk "test")
    "t b" 'cider-test-show-report
    "t n" 'cider-test-run-ns-tests
    "t p" 'cider-test-run-project-tests
    "t t" 'cider-test-run-test))

(use-package flycheck-clj-kondo)

;; TODO this should probably go somewhere else, if anywhere
(defun sulami/clojure-thread-last ()
  "Unwraps an onion of functions into a thread-last macro.

Place point on the outer-most opening parenthesis to start:
|(f (g (h x))) => (->> x (h) (g) (f))"
  (interactive)
  (let ((start (point))
        (depth 0))

    (while (let ((pos (point)))
             (sp-down-sexp)
             (not (= pos (point))))
      (setq depth (+ 1 depth)))

    (goto-char start)
    (sp-down-sexp)

    (--dotimes depth
      (sp-forward-barf-sexp)
      (left-char)
      (sp-kill-sexp)
      (right-char))

    (re-search-forward "\n" nil t)
    (left-char)

    (--each (-take depth kill-ring)
      (insert (format " %s" it)))

    (goto-char start)
    (insert "(->>) ")
    (goto-char (+ 1 start))
    (sp-forward-slurp-sexp (+ 1 depth))
    (goto-char start)))

(use-package haskell-mode
  :defer t)

;;; Fix indentation when using o/O in Haskell
;(defun haskell-evil-open-above ()
;  (interactive)
;  (evil-digit-argument-or-evil-beginning-of-line)
;  (haskell-indentation-newline-and-indent)
;  (evil-previous-line)
;  (haskell-indentation-indent-line)
;  (evil-append-line nil))
;
;(defun haskell-evil-open-below ()
;  (interactive)
;  (evil-append-line nil)
;  (haskell-indentation-newline-and-indent))
;
;(evil-define-key 'normal haskell-mode-map
;  "o" 'haskell-evil-open-below
;  "O" 'haskell-evil-open-above)

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)))

(use-package yaml-mode
  :defer t)

;; Indent by 2 spaces, if we ever get there
(setq js2-basic-offset 2)

(use-package protobuf-mode
  :defer t)

(add-hook 'emacs-startup-hook
          (lambda ()
            (let ((pkg-count (length (hash-table-keys straight--success-cache)))
                  (startup-time (float-time (time-subtract after-init-time before-init-time))))
              (message (format "Startup complete, loaded %d packages in %.2fs"
                               pkg-count
                               startup-time)))))
