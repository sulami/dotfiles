;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     elixir
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
     javascript
     latex
     markdown
     nginx
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
                                      jbeans-theme
                                      jinja2-mode
                                      json-mode
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
                                    ;; org-bullets
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
   dotspacemacs-startup-banner 'random
   dotspacemacs-startup-lists '(recents bookmarks projects)
   dotspacemacs-startup-recent-list-size 5
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   dotspacemacs-themes '(jbeans
                         solarized-dark
                         solarized-light)
   dotspacemacs-colorize-cursor-according-to-state t
   dotspacemacs-default-font '("Inconsolata"
                               :size 14
                               :weight regular
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
   dotspacemacs-line-numbers '(:relative t
                               :enabled-for-modes nil)
   dotspacemacs-smartparens-strict-mode nil
   dotspacemacs-highlight-delimiters 'all
   dotspacemacs-persistent-server nil
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   dotspacemacs-default-package-repository nil
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
   It is called immediately after `dotspacemacs/init'.  You are free to put almost
   any user code here.  The exception is org related code, which should be placed
   in `dotspacemacs/user-config'."

  (setq
   ;; Default frame size
   default-frame-alist '((width . 120) (height . 40) (scroll-bar-mode . nil))
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
  ;; (when (not (buffer-file-name))
  ;;   (desktop-read))

  ;; ;; Set the colourscheme according to the time of day
  ;; (let ((hour-of-day (read (format-time-string "%H"))))
  ;;   (if (< 6 hour-of-day 20)
  ;;     (load-theme 'solarized-dark)
  ;;     (load-theme 'solarized-dark)))
  (load-theme 'jbeans)

  ;; Set all kinds of stuff
  (setq
   ;; Hide the clutter
   backup-directory-alist '("/tmp/emacs-backup")
   ;; No trash
   delete-by-moving-to-trash nil
   ;; Fix scroll speed
   mouse-wheel-progressive-speed nil
   mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil))
   ;; Un-fancy the modeline
   powerline-default-separator nil
   ;; Set helm to fuzzy matching
   helm-mode-fuzzy-match t
   ;; Hide markup in org-mode
   org-hide-emphasis-markers t
   ;; Disable non-stack GHC in Flycheck
   flycheck-disabled-checkers '(haskell-ghc)
   )

  ;; Custom session save directory
  (defun emacs-session-filename (session-id)
    "Construct a filename to save the session in based on SESSION-ID. Customized
    version that saves to /tmp."
    (let ((basename (concat "session." session-id)))
      (concat "/tmp/.emacs-" basename)))

  ;; Jump "in" using ctrl-i
  (custom-set-variables
   '(evil-want-C-i-jump t))

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

  (require 'company)

  ;; Use vim-style keys for autocompletion
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  ;; Fix C-w when autocompleting
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)

  ;; Map snippets from insert mode
  (define-key evil-insert-state-map (kbd "C-y") 'spacemacs/helm-yas)

  ;; ;; Enable evil-smartparens-mode in lisp modes
  ;; (add-hook 'smartparens-enabled-hook
  ;;           #'(lambda ()
  ;;               (when common-lisp-mo
  ;;                 (evil-smartparens-mode))))

  ;; Set helm to fuzzy matching and fix c-w
  (require 'helm)
  (define-key helm-map (kbd "C-w") 'evil-delete-backward-word)

  ;; Add dropdown completion for common lisp
  (slime-setup '(slime-company))
  ;; This is dirty, but I cannot bring slime to not load slime-fuzzy
  (defun slime-fuzzy-complete-symbol ()
    (interactive)
    nil)

  ;; Don't interrupt me if autocompletion falls over
  (remove-hook 'anaconda-mode-response-read-fail-hook
               'anaconda-mode-show-unreadable-response)

  ;; Enable autocompletion for C
  (add-hook 'c-mode-hook 'company-mode)

  ;; Spell-checking in org-mode
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; Enable refill mode for Markdown
  ;; (add-hook 'markdown-mode-hook 'refill-mode)

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
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-safe-themes
   (quote
    ("64f2981274fd8740b794bce9feee7949ed87b88fc0b4654bd98594e1aa81abcd" "45712b65018922c9173439d9b1b193cb406f725f14d02c8c33e0d2cdad844613" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(evil-want-C-i-jump t)
 '(evil-want-Y-yank-to-eol t)
 '(linum-format " %3i ")
 '(magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))
 '(package-selected-packages
   (quote
    (winum org-category-capture fuzzy flycheck-credo colemak-evil docker-api dockerfile-mode docker php-auto-yasnippets drupal-mode phpunit phpcbf php-extras php+-mode php-mode epresent org-present nyan-mode ob-elixir flycheck-mix alchemist web-beautify livid-mode skewer-mode simple-httpd js2-refactor js2-mode js-doc company-tern dash-functional tern coffee-mode apib-mode flycheck-elixir color-theme-solarized nginx-mode rust-mode elixir-mode jebans-theme jbeans-theme smooth-scroll org-bullets yapfify yaml-mode xterm-color ws-butler window-numbering which-key web-mode volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tagedit spacemacs-theme spaceline powerline slime-company slime slim-mode shell-pop scss-mode sass-mode restart-emacs rainbow-delimiters racket-mode faceup pyvenv pytest pyenv-mode py-isort pug-mode popwin pony-mode pip-requirements persp-mode pcre2el paradox orgit org org-projectile org-pomodoro alert log4e gntp org-plus-contrib org-download open-junk-file multi-term move-text mmm-mode markdown-toc markdown-mode magit-gitflow magit-gh-pulls macrostep lorem-ipsum live-py-mode linum-relative link-hint less-css-mode json-mode json-snatcher json-reformat jinja2-mode intero info+ indent-guide hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers parent-mode highlight-indentation hide-comnt help-fns+ helm-themes helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make projectile helm-hoogle helm-gitignore request helm-flx flx helm-descbinds helm-css-scss helm-cscope xcscope helm-company helm-c-yasnippet helm-ag haskell-snippets haml-mode google-translate golden-ratio go-guru go-eldoc gnuplot gitignore-mode github-search github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht gh-md geiser flyspell-correct-helm helm helm-core flyspell-correct flycheck-pos-tip pos-tip flycheck-haskell flycheck fill-column-indicator eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-surround evil-smartparens evil-search-highlight-persist evil-numbers evil-matchit evil-magit magit magit-popup git-commit with-editor evil-lisp-state smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-escape evil-ediff evil-commentary evil-args evil-anzu anzu evil goto-chg undo-tree eshell-z eshell-prompt-extras esh-help elisp-slime-nav dumb-jump popup disaster diminish diff-hl define-word cython-mode company-web web-completion-data company-statistics company-go go-mode company-ghci company-ghc ghc haskell-mode company-cabal company-c-headers company-auctex company-anaconda company common-lisp-snippets column-enforce-mode cmm-mode cmake-mode clojure-snippets clj-refactor hydra inflections edn multiple-cursors paredit peg clean-aindent-mode clang-format cider-eval-sexp-fu eval-sexp-fu highlight cider seq spinner queue pkg-info clojure-mode epl bind-map bind-key auto-yasnippet yasnippet auto-highlight-symbol auto-dictionary auto-compile packed auctex async anaconda-mode pythonic f s aggressive-indent adaptive-wrap ace-window ace-link avy quelpa package-build solarized-theme dash))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
