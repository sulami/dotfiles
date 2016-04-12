;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t)
     c-c++
     clojure
     common-lisp
     cscope
     django
     emacs-lisp
     evil-commentary
     git
     github
     go
     haskell
     html
     latex
     markdown
     org
     (python :variables
             python-enable-yapf-format-on-save t
             python-test-runner 'pytest)
     racket
     scheme
     (shell :variables
            shell-default-shell 'eshell
            shell-enable-smart-eshell nil
            shell-protect-eshell-prompt t)
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     syntax-checking
     version-control
     yaml
     )
   dotspacemacs-additional-packages '(company-c-headers
                                      evil-smartparens
                                      slime-company)
   dotspacemacs-excluded-packages '(ac-ispell
                                    ace-jump-helm-line
                                    auto-complete
                                    emmet-mode
                                    evil-jumper
                                    evil-mc
                                    evil-tutor
                                    fancy-battery
                                    flx-ido
                                    ido-vertical-mode
                                    leuven-theme
                                    neotree
                                    org-bullets
                                    org-present
                                    smeargle)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  (setq-default
   dotspacemacs-elpa-https t
   dotspacemacs-elpa-timeout 5
   dotspacemacs-check-for-update nil
   dotspacemacs-editing-style 'vim
   dotspacemacs-verbose-loading nil
   dotspacemacs-startup-banner nil
   dotspacemacs-startup-lists nil ;;'(recents bookmarks projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   dotspacemacs-themes '(solarized-dark
                         solarized-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Inconsolata"
                               :size 15
                               :weight bold
                               :width normal
                               :powerline-scale 1.1)
   dotspacemacs-leader-key "SPC"
   dotspacemacs-emacs-leader-key "M-m"
   dotspacemacs-major-mode-leader-key ","
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   dotspacemacs-distinguish-gui-tab nil
   dotspacemacs-command-key ":"
   dotspacemacs-remap-Y-to-y$ t
   dotspacemacs-default-layout-name "Default"
   dotspacemacs-display-default-layout nil
   dotspacemacs-auto-resume-layouts nil
   dotspacemacs-auto-save-file-location 'cache
   dotspacemacs-max-rollback-slots 5
   dotspacemacs-use-ido nil
   dotspacemacs-helm-resize nil
   dotspacemacs-helm-no-header nil
   dotspacemacs-helm-position 'bottom
   dotspacemacs-enable-paste-micro-state nil
   dotspacemacs-which-key-delay 0.4
   dotspacemacs-which-key-position 'bottom
   dotspacemacs-loading-progress-bar nil
   dotspacemacs-fullscreen-at-startup nil
   dotspacemacs-fullscreen-use-non-native nil
   dotspacemacs-maximized-at-startup nil
   dotspacemacs-active-transparency 90
   dotspacemacs-inactive-transparency 90
   dotspacemacs-mode-line-unicode-symbols nil
   dotspacemacs-smooth-scrolling t
   dotspacemacs-line-numbers nil
   dotspacemacs-smartparens-strict-mode t
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
   It is called immediately after `dotspacemacs/init'.  You are free to put almost
   any user code here.  The exception is org related code, which should be placed
   in `dotspacemacs/user-config'."

  (setq
   ;; Default frame size
   default-frame-alist '((width . 85) (scroll-bar-mode . nil))
   ;; Prevent enormous lag during startup
   tramp-ssh-controlmaster-options
    "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o Control-Persist=no"
   )

  ;; Kill buffers when exiting emacsclient
  (add-hook 'server-done-hook 'kill-buffer)

  ;; Mail-mode for everything mutt
  (add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

  ;; Fix scrollbars in newly created frames
  (add-to-list 'after-make-frame-functions
               (lambda (arg)
                 (menu-bar-no-scroll-bar)))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code. This function is called at the very end
   of Spacemacs initialization after layers configuration. You are free to put
   any user code."

  ;; Load my default starting desktop if started without file input
  (when (not (buffer-file-name))
    (desktop-read))

  ;; Set the colourscheme according to the time of day
  (let ((hour-of-day (read (format-time-string "%H"))))
    (if (< 6 hour-of-day 20)
      (load-theme 'solarized-light)
      (load-theme 'solarized-dark)))

  ;; Set all kinds of stuff
  (setq
   ;; Hide the clutter
   backup-directory-alist '("/tmp/emacs-backup")
   ;; No trash
   delete-by-moving-to-trash nil
   ;; Un-fancy the modeline
   powerline-default-separator nil
   ;; Set helm to fuzzy matching
   helm-mode-fuzzy-match t
   ;; Disable non-stack GHC in Flycheck
   flycheck-disabled-checkers '(haskell-ghc)
   )

  ;; Custom session save directory
  (defun emacs-session-filename (session-id)
    "Construct a filename to save the session in based on SESSION-ID. Customized
    version that saves to /tmp."
    (let ((basename (concat "session." session-id)))
      (concat "/tmp/.emacs-" basename)))

  ;; Apropos
  (spacemacs/set-leader-keys "ha" 'helm-apropos)

  ;; Proper in-/decrease
  (define-key evil-normal-state-map (kbd "C-1")
    'spacemacs/evil-numbers-decrease)
  (define-key evil-normal-state-map (kbd "C-2")
    'spacemacs/evil-numbers-increase)

  ;; Adjust the font size on the fly
  (define-key evil-normal-state-map (kbd "C-0")
    'spacemacs/reset-font-size)
  (define-key evil-normal-state-map (kbd "C--")
    'spacemacs/scale-down-font)
  (define-key evil-normal-state-map (kbd "C-=")
    'spacemacs/scale-up-font)

  ;; More convenient than C-x #
  (spacemacs/set-leader-keys "qw" 'server-edit)

  ;; Clear highlight with return
  (defun isearch-nohighlight ()
    "Remove search highlights if not in the isearch minor mode."
    (interactive)
    (when (not isearch-mode)
      (evil-search-highlight-persist-remove-all)))
  (define-key evil-normal-state-map (kbd "RET") 'isearch-nohighlight)

  ;; If inside a project, pop shells in the project root
  (defun project-root-shell ()
    "Pop the default shell in the project root if inside a project, otherwise in
the default directory"
    (interactive)
    (if (projectile-project-p)
        (projectile-with-default-dir (projectile-project-root)
          (spacemacs/default-pop-shell))
      (spacemacs/default-pop-shell)))
  (spacemacs/set-leader-keys "'" 'project-root-shell)
  ;; This "unpops" the shell without having to leave insert mode
  (define-key evil-insert-state-map (kbd "C-c '") 'project-root-shell)

  ;; Fix C-w when autocompleting
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)

  ;; Enable evil-smartparens-mode when using smartparens outside of markdown and
  ;; python
  (add-hook 'smartparens-enabled-hook
            #'(lambda ()
                (when (not (or spacemacs-markdown-mode-map-active
                               spacemacs-python-mode-map-active))
                  (evil-smartparens-mode))))

  ;; Set helm to fuzzy matching and fix c-w
  (require 'helm)
  (define-key helm-map (kbd "C-w") 'evil-delete-backward-word)

  ;; Add dropdown completion for common lisp
  (slime-setup '(slime-company))
  ;; This is dirty, but I cannot bring slime to not load slime-fuzzy
  (defun slime-fuzzy-complete-symbol ()
    (interactive)
    nil)

  ;; Enable autocompletion for C
  (add-hook 'c-mode-hook 'company-mode)

  ;; Spell-checking in org-mode
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; Enable refill mode for Markdown
  (add-hook 'markdown-mode-hook 'refill-mode)

  ;; Sprunge support
  (defun sprunge-buffer ()
    (interactive)
    (shell-command-on-region
     (point-min) (point-max)
     "curl -sF 'sprunge=<-' http://sprunge.us"
     "*Sprunge*")
    (with-current-buffer "*Sprunge*"
      (message (concat "Sprunged to " (buffer-string)))
      (spacemacs/copy-whole-buffer-to-clipboard)))
  (spacemacs/set-leader-keys "b S" 'sprunge-buffer)
  )


;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(magit-merge-arguments (quote ("--no-ff"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
