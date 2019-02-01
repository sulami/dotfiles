;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defvar sulami/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
;; Customisations live in a separate file
(setq custom-file "~/dotfiles/custom.el")

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
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t)
     ;; c-c++
     clojure
     ;; common-lisp
     ;; cscope
     ;; django
     docker
     elixir
     emacs-lisp
     evil-cleverparens
     evil-commentary
     git
     github
     ;; go
     gnus
     haskell
     helm
     html
     javascript
     ;; latex
     markdown
     nginx
     org
     (python :variables
             python-enable-yapf-format-on-save t
             python-test-runner 'pytest)
     ;; racket
     restclient
     ;; ruby
     ;; scheme
     (shell :variables
            shell-default-shell 'eshell
            shell-enable-smart-eshell nil
            shell-protect-eshell-prompt t)
     (spell-checking :variables
                     spell-checking-enable-by-default nil)
     ;; swift
     syntax-checking
     ;; vagrant
     ;; version-control
     yaml
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(apib-mode
                                      ;; atomic-chrome
                                      ;; company-c-headers
                                      ;; darktooth-theme
                                      ;; jbeans-theme
                                      ;; jinja2-mode
                                      json-mode
                                      gruvbox-theme
                                      ;; magithub
                                      ;; slime-company
                                      ;; tramp-term
                                      )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '(ac-ispell
                                    ace-jump-helm-line
                                    auto-complete
                                    docker
                                    docker-tramp
                                    emmet-mode
                                    evil-jumper
                                    evil-mc
                                    evil-tutor
                                    fancy-battery
                                    flx-ido
                                    ido-vertical-mode
                                    leuven-theme
                                    magit-gitflow
                                    neotree
                                    org-bullets
                                    smeargle)
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner '999
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (bookmarks . 10)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'emacs-lisp-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(gruvbox-dark-soft
                         gruvbox-light-soft)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Code"
                               :size 12
                               :weight regular
                               :width normal
                               :powerline-scale 1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key ":"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ t
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers '(:relative t
                               :enabled-for-modes nil)
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup 'trailing
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (require 'ido)
  (ido-mode -1)

  ;; Load these for gruvbox at startup
  (mapc 'load (file-expand-wildcards `"~/.emacs.d/elpa/dash-*/dash.el"))
  (mapc 'load (file-expand-wildcards "~/.emacs.d/elpa/autothemer-*/autothemer.el"))

  (setq
   ;; I know I'm setting environment variables in my .zshrc
   exec-path-from-shell-check-startup-files nil
   ;; Default frame size
   default-frame-alist '((width . 130) (height . 80) (scroll-bar-mode . nil))
   ;; Prevent enormous lag during startup
   tramp-ssh-controlmaster-options
    "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o Control-Persist=no"
   )

  (setq gnus-select-method
        '(nnmaildir ""
                    (directory "/Users/robinschroer/mail")
                    (get-new-mail nil)))
  (setq mail-sources nil)
  (setq gnus-secondary-select-methods nil)

  ;; Kill buffers when exiting emacsclient
  (add-hook 'server-done-hook 'kill-buffer)

  ;; Mail-mode for everything mutt
  (add-to-list 'auto-mode-alist '("/mutt" . mail-mode))

  ;; Fix scrollbars in newly created frames
  (add-to-list 'after-make-frame-functions
               (lambda (arg)
                 (menu-bar-no-scroll-bar)))

  ;; Window navigation using super instead of <Leader>
  (setq winum-keymap
        (let ((map (make-sparse-keymap)))
          (define-key map (kbd "s-0") 'winum-select-window-0-or-10)
          (define-key map (kbd "s-1") 'winum-select-window-1)
          (define-key map (kbd "s-2") 'winum-select-window-2)
          (define-key map (kbd "s-3") 'winum-select-window-3)
          (define-key map (kbd "s-4") 'winum-select-window-4)
          (define-key map (kbd "s-5") 'winum-select-window-5)
          (define-key map (kbd "s-6") 'winum-select-window-6)
          (define-key map (kbd "s-7") 'winum-select-window-7)
          (define-key map (kbd "s-8") 'winum-select-window-8)
          (define-key map (kbd "s-9") 'winum-select-window-9)
          map))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  ;; Load my default starting desktop if started without file input
  ;; (when (not (buffer-file-name))
  ;;   (desktop-read))

  ;; Set the default colourscheme according to the time of day
  ;; (let ((hour-of-day (read (format-time-string "%H"))))
  ;;   (if (< 9 hour-of-day 18)
  ;;       (load-theme 'gruvbox-light-soft)
  ;;     (load-theme 'gruvbox-dark-soft)))

  (add-to-list 'load-path "~/dotfiles/")
  (load "sulami")

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
   ;; Hide markup in org-mode
   org-hide-emphasis-markers t
   ;; Store org-journal journals in iCloud
   org-journal-dir "~/Documents/org-journal/"
   ;; Store junk files in /tmp
   open-junk-file-directory "/tmp/emacs-junk/%Y-%m-%d-%H%M%S."
   ;; Use aspell for spelling, with British spelling
   ispell-program-name "aspell"
   ispell-extra-args (quote ("--sug-mode=ultra" "--lang=en_GB-ise"))
   ;; Disable non-stack GHC in Flycheck
   flycheck-disabled-checkers '(haskell-ghc))

  ;; Disable current line highlight
  (spacemacs/toggle-highlight-current-line-globally-off)

  (add-hook 'after-make-frame-functions 'sulami/setup-frame)

  ;; Custom session save directory
  (defun emacs-session-filename (session-id)
    "Construct a filename to save the session in based on SESSION-ID. Customized
    version that saves to /tmp."
    (let ((basename (concat "session." session-id)))
      (concat "/tmp/.emacs-" basename)))

  ;; Jump "in" using ctrl-i
  (custom-set-variables '(evil-want-C-i-jump t))

  ;; Prevent evil-cleverparens from converting >/< to slurp/barf
  (defun sulami/evil-cp-modify-regular-bindings (&rest r)
    (setq evil-cp-regular-bindings
          (remove-if (lambda (key-string)
                       (member key-string '("_" ">" "<")))
                     evil-cp-regular-bindings
                     :key 'car)))
  (advice-add 'evil-cp--enable-regular-bindings :before
              #'sulami/evil-cp-modify-regular-bindings)

  ;; Enable evil-cleverparens
  (setq evil-cleverparens-use-additional-movement-keys nil)
  (setq evil-cleverparens-use-additional-bindings nil)
  (spacemacs/toggle-evil-cleverparens-on)
  (add-hook 'smartparens-enabled-hook 'evil-cleverparens-mode)

  ;; Quick window layout
  (spacemacs/set-leader-keys "w4" 'sulami/layout-triple-fib)

  ;; Apropos
  (spacemacs/set-leader-keys "ha" 'helm-apropos)

  ;; Open the message buffer
  (spacemacs/set-leader-keys "bm" 'sulami/open-message-buffer)

  ;; Open my note file
  (spacemacs/set-leader-keys "ft"
    (lambda ()
      (interactive)
      (sulami/pop-file "~/Documents/TODO.org")))

  ;; Proper in-/decrease
  (define-key evil-normal-state-map (kbd "C-1")
    'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "C-2")
    'evil-numbers/inc-at-pt)

  ;; Adjust the font size on the fly
  (define-key evil-normal-state-map (kbd "C-0")
    'spacemacs/reset-font-size)
  (define-key evil-normal-state-map (kbd "C--")
    'spacemacs/scale-down-font)
  (define-key evil-normal-state-map (kbd "C-=")
    'spacemacs/scale-up-font)

  ;; More convenient than C-x #
  (spacemacs/set-leader-keys "qw" 'server-edit)

  ;; Trigger jump to word
  (define-key evil-normal-state-map (kbd "s-n") 'avy-goto-word-or-subword-1)

  ;; De-prettify org-mode
  (add-hook 'org-mode-hook 'sulami/org-mode-format)

  ;; Enable org-indent-mode
  (add-hook 'org-mode-hook 'org-indent-mode)

  ;; Import markdown to org-mode
  ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "em" 'sulami/markdown-to-org-mode)

  ;; Disable flycheck on-the-fly checking for performance in large Python files
  (add-hook 'python-mode-hook (lambda () (sulami/flycheck-disable-for-large-files 2000)))

  ;; Shortcuts to run single Python tests
  (spacemacs/set-leader-keys-for-major-mode 'python-mode "tc" 'sulami/python-copy-current-test)
  (spacemacs/set-leader-keys-for-major-mode 'python-mode "tr" 'sulami/python-run-current-test)

  ;; Shortcut to open the test buffer
  (spacemacs/set-leader-keys "bt" 'sulami/open-test-buffer)

  ;; Clear highlight with return
  (define-key evil-normal-state-map (kbd "RET") 'sulami/isearch-nohighlight)

  ;; If inside a project, pop shells in the project root
  (define-key global-map (kbd "s-'") 'sulami/project-root-shell)

  ;; Kill the scratch frame
  (spacemacs/set-leader-keys "qy" 'sulami/kill-scratch-frame)

  ;; Terminals live in permanent holy mode
  (evil-set-initial-state 'term-mode 'emacs)

  (when (require 'term nil t) ; only if term can be loaded..
    (setq term-bind-key-alist
          (list (cons "C-c C-c" 'term-interrupt-subjob)
                (cons "C-p" 'previous-line)
                (cons "C-n" 'next-line)
                (cons "M-f" 'term-send-forward-word)
                (cons "M-b" 'term-send-backward-word)
                (cons "C-c C-j" 'term-line-mode)
                (cons "C-c C-k" 'term-char-mode)
                (cons "M-DEL" 'term-send-backward-kill-word)
                (cons "M-d" 'term-send-forward-kill-word)
                (cons "<C-left>" 'term-send-backward-word)
                (cons "<C-right>" 'term-send-forward-word)
                (cons "C-r" 'term-send-reverse-search-history)
                (cons "M-p" 'term-send-raw-meta)
                (cons "M-y" 'term-send-raw-meta)
                (cons "C-y" 'term-send-raw))))

  (require 'company)

  ;; Use vim-style keys for autocompletion
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)

  ;; Fix C-w when autocompleting
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)

  ;; Map snippets from insert mode
  (define-key evil-insert-state-map (kbd "C-y") 'spacemacs/helm-yas)

  ;; Set helm to fuzzy matching and fix c-w
  (require 'helm)
  (define-key helm-map (kbd "C-w") 'evil-delete-backward-word)

  ;; Fix C-w when swooping
  ;; (require 'helm-swoop)
  ;; (define-key helm-swoop-map (kbd "C-w") 'evil-delete-backward-word)

  ;; Add dropdown completion for common lisp
  ;; (slime-setup '(slime-company))
  ;; This is dirty, but I cannot bring slime to not load slime-fuzzy
  ;; (defun slime-fuzzy-complete-symbol ()
  ;;   (interactive)
  ;;   nil)

  ;; Fix indentation when using o/O in Haskell
  (defun haskell-evil-open-above ()
    (interactive)
    (evil-digit-argument-or-evil-beginning-of-line)
    (haskell-indentation-newline-and-indent)
    (evil-previous-line)
    (haskell-indentation-indent-line)
    (evil-append-line nil))

  (defun haskell-evil-open-below ()
    (interactive)
    (evil-append-line nil)
    (haskell-indentation-newline-and-indent))

  (evil-define-key 'normal haskell-mode-map
    "o" 'haskell-evil-open-below
    "O" 'haskell-evil-open-above)

  ;; Don't interrupt me if autocompletion falls over
  (remove-hook 'anaconda-mode-response-read-fail-hook
               'anaconda-mode-show-unreadable-response)

  ;; Enable autocompletion for C
  (add-hook 'c-mode-hook 'company-mode)

  ;; Spell-checking in org-mode
  (add-hook 'org-mode-hook 'flyspell-mode)

  ;; Enable refill mode for Markdown
  ;; (add-hook 'markdown-mode-hook 'refill-mode)

  ;; Sprunge doesn't really work any more, needs to be replaced
  ;; (spacemacs/set-leader-keys "b S" 'sulami/sprunge-buffer)

  ;; Ligature support, source: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
  (global-auto-composition-mode 0)
  (let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
                 (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
                 (36 . ".\\(?:>\\)")
                 (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
                 (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
                 (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
                 (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
                 (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
                 (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
                 (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
                 (48 . ".\\(?:x[a-zA-Z]\\)")
                 (58 . ".\\(?:::\\|[:=]\\)")
                 (59 . ".\\(?:;;\\|;\\)")
                 (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
                 (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
                 (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
                 (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
                 (91 . ".\\(?:]\\)")
                 (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
                 (94 . ".\\(?:=\\)")
                 (119 . ".\\(?:ww\\)")
                 (123 . ".\\(?:-\\)")
                 (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
                 (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
    (dolist (char-regexp alist)
      (set-char-table-range composition-function-table (car char-regexp)
                            `([,(cdr char-regexp) 0 font-shape-gstring]))))

  (defun file-composition-mode ()
    "Enable `auto-composition-mode` if the buffer is visiting a file."
    (if (buffer-file-name)
      (auto-composition-mode)))
  (dolist (mode-hook '(text-mode-hook prog-mode-hook))
    (add-hook mode-hook 'file-composition-mode))

  ;; Bring up the atomic-chrome editing server
  ;; (require 'atomic-chrome)
  ;; (atomic-chrome-start-server)
  ;; (setq atomic-chrome-default-major-mode 'markdown-mode
  ;;       atomic-chrome-buffer-open-style 'frame)

  (load-file custom-file)

  (setq file-name-handler-alist sulami/file-name-handler-alist))
