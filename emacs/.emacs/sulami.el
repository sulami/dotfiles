;; GENERAL STUFF

(defun sulami/is-spacemacs ()
  "Returns t if running in Spacemacs."
  (let ((rv (and (boundp 'is-spacemacs)
                 is-spacemacs)))
    (unless rv
      (message "Not running in Spacemacs, skipping"))
    rv))

(defun sulami/layout-triple-fib ()
  "Open one window on the left and stacked on the right."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (select-window (next-window))
  (split-window-vertically))

(defun sulami/pop-file (file-path)
  "Pop up a buffer with a file path."
  (popwin:popup-buffer (find-file-noselect file-path)
                       :position :bottom
                       :stick t))

(defun sulami/open-message-buffer ()
  "Open the message buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun sulami/open-test-buffer ()
  "Open the test buffer."
  (interactive)
  (switch-to-buffer "*Test*"))

(defun sulami/sprunge-buffer ()
  "Send the current buffer content to sprunge.us and copy the URL to the
clipboard."
  (interactive)
  (when (sulami/is-spacemacs)
    (shell-command-on-region
     (point-min) (point-max)
     "curl -sF 'sprunge=<-' http://sprunge.us"
     "*Sprunge*")
    (with-current-buffer "*Sprunge*"
      (message (concat "Sprunged to " (buffer-string)))
      (spacemacs/copy-whole-buffer-to-clipboard))))

(defun sulami/save-to-junk ()
  "Save the current buffer as a junk file."
  (interactive)
  (when (sulami/is-spacemacs)
    (spacemacs/copy-whole-buffer-to-clipboard)
    (kill-buffer)
    (open-junk-file)
    (insert (current-kill 0))
    (save-buffer)))

(defun sulami/kill-project-layout ()
  "Kill the current project and then the layout."
  (interactive)
  (when (sulami/is-spacemacs)
    (projectile-kill-buffers)
    (spacemacs/layouts-ts-kill)))

(defun sulami/init-modeline ()
  "Start up my modeline."
  (require 'doom-modeline)
  (doom-modeline-mode 1)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-persp-name nil))

;; ORG

(defun sulami/org-mode-format ()
  "Stop the org-level headers from being fancy."
  (interactive)
  (auto-composition-mode 0)
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5
                  org-level-6
                  org-level-7
                  org-level-8))
    (set-face-attribute face nil :inherit :family :weight 'normal :height 1.0)))

(defun sulami/markdown-to-org-mode ()
  "Markdown -> org-mode link conversion."
  (interactive)
  (evil-ex "%s/\\[\\(.*\\)\\](\\(.*\\))/[[\\2][\\1]]/g"))

;; PYTHON

(defun sulami/python-get-current-test ()
  "Get the current test path for pytest."
  (interactive)

  (cl-flet
      ((get-test-name
        ()
        (let* ((reset-point (point))
               (def-start (search-backward "def "))
               (_ (forward-char (length "def ")))
               (name-start (point))
               (name-end (- (search-forward "(") 1))
               (rv (buffer-substring name-start name-end)))
          (goto-char reset-point)
          rv))

      (get-class-name
        ()
        (let ((reset-point (point))
              (def-start (search-backward "class " nil t)))
          (if def-start
              (progn
                (forward-char (length "class "))
                (let* ((name-start (point))
                       (name-end (- (search-forward "(") 1))
                       (rv (buffer-substring name-start name-end)))
                  (goto-char reset-point)
                  (concatenate 'string rv "::")))
            "")))

      (get-test-path
        ()
        (let* ((splitted-path (split-string buffer-file-name "/"))
               (magic-headoff (nthcdr 5 splitted-path)))
          (joined (mapconcat 'identity magic-headoff "/")))))

    (concatenate 'string (get-test-path) "::" (get-class-name) (get-test-name))))

(defun sulami/python-run-test (test-path)
  "Run a test inside Docker."
  (let ((temp-buffer-name "*Test*")
        (inhibit-read-only t))
    (get-buffer-create temp-buffer-name)
    (let* ((test-command (concatenate 'string
                                      (projectile-project-root)
                                      "test "
                                      test-path))
            (process (start-process-shell-command "Test"
                                                  temp-buffer-name
                                                  test-command)))
      (with-current-buffer temp-buffer-name
        (erase-buffer)
        (require 'shell)
        (shell-mode)
        (set-process-filter process 'comint-output-filter)))
    (let ((temp-buffer-window (get-buffer-window temp-buffer-name)))
      (if temp-buffer-window
          (select-window temp-buffer-window)
        (popwin:popup-buffer temp-buffer-name
                              :position :bottom)))))

(defun sulami/python-run-current-test ()
  "Run the current test inside Docker."
  (interactive)
  (sulami/python-run-test (sulami/python-get-current-test)))

(defun sulami/python-copy-current-test ()
  "Copy the current test path for pytest."
  (interactive)
  (kill-new (sulami/python-get-current-test)))

;; INTERNALS

(defun sulami/setup-frame (&optional frame)
  "Set some options depending on whether we're in GUI or terminal mode."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
                       (if (display-graphic-p frame)
                           1 0)))

(defun sulami/isearch-nohighlight ()
  "Remove search highlights if not in the isearch minor mode."
  (interactive)
  (when (not isearch-mode)
    (evil-search-highlight-persist-remove-all)))

(defun sulami/buffer-line-count ()
  "Get the number of lines in the active buffer."
  (count-lines 1 (point-max)))

(defun sulami/flycheck-disable-for-large-files (limit)
  "Disable flycheck on-the-fly-checking if the line count exceeds LIMIT."
  (setq flycheck-check-syntax-automatically
        (if (> (sulami/buffer-line-count) limit)
            (delete 'idle-change flycheck-check-syntax-automatically)
          (add-to-list 'flycheck-check-syntax-automatically 'idle-change))))

(defun sulami/project-root-shell ()
  "Pop the default shell in the project root if inside a project, otherwise in
the default directory."
  (interactive)
  (when (sulami/is-spacemacs)
    (if (projectile-project-p)
        (projectile-with-default-dir (projectile-project-root)
          (spacemacs/default-pop-shell))
      (spacemacs/default-pop-shell))))

(defun sulami/magit-status-same-window ()
  "Open the magit status in the current window."
  (interactive)
  (let ((magit-display-buffer-function
         (lambda (buffer)
           (display-buffer buffer '(display-buffer-same-window)))))
    (magit-status)))

(defun sulami/scratch-frame ()
  "Open empty scratch buffer in new frame.

To be called from the outside using `emacsclient -a '' -e
\"(sulami/scratch-frame)\"`."
  (when (sulami/is-spacemacs)
    (switch-to-buffer-other-frame "*scratch*")
    (spacemacs/toggle-maximize-buffer)
    (if (< 0 (buffer-size))
        (spacemacs/safe-erase-buffer))))

(defun sulami/kill-scratch-frame ()
  "Copy the content of the current buffer, empty it and kill the frame."
  (interactive)
  (when (sulami/is-spacemacs)
    (clipboard-kill-ring-save (point-min) (point-max))
    (erase-buffer)
    (spacemacs/frame-killer)))

;; CONFIG

(defun sulami/config ()
  "Setup all my personal config."

  (require 'ido)
  (ido-mode -1)

  ;; Load these for gruvbox at startup
  (mapc 'load (file-expand-wildcards `"~/.emacs.d/elpa/dash-*/dash.el"))
  (mapc 'load (file-expand-wildcards "~/.emacs.d/elpa/autothemer-*/autothemer.el"))

  (setq
   ;; This is needed for evil-collection to work properly later on
   evil-want-keybinding nil
   ;; I know I'm setting environment variables in my .zshrc
   exec-path-from-shell-check-startup-files nil
   ;; Default frame size
   default-frame-alist '((width . 100) (height . 40) (scroll-bar-mode . nil))
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

  ;; Load my default starting desktop if started without file input
  ;; (when (not (buffer-file-name))
  ;;   (desktop-read))

  ;; Set the default colourscheme according to the time of day
  ;; (let ((hour-of-day (read (format-time-string "%H"))))
  ;;   (if (< 9 hour-of-day 18)
  ;;       (load-theme 'gruvbox-light-soft)
  ;;     (load-theme 'gruvbox-dark-soft)))

  ;; Set all kinds of stuff
  (setq
   ;; Undo the modifier key swapping from macports emacs
   mac-command-modifier 'super
   mac-option-modifier 'meta
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
   ;; All notes are agenda files
   org-agenda-files '("~/Documents/Notes/")
   ;; Store junk files in /tmp
   open-junk-file-directory "/tmp/emacs-junk/%Y-%m-%d-%H%M%S."
   ;; Use aspell for spelling, with British spelling
   ispell-program-name "aspell"
   ispell-extra-args (quote ("--sug-mode=ultra" "--lang=en_GB-ise"))
   ;; Custom yasnippet directory
   yas-snippet-dirs (cons "~/.emacs/snippets/" yas-snippet-dirs)
   ;; Disable non-stack GHC in Flycheck
   flycheck-disabled-checkers '(haskell-ghc)
   ;; Indent JS(ON) by 2 spaces
   js2-basic-offset 2)

  ;; :nowrap by default
  (set-default 'truncate-lines t)

  ;; Fix paste with external tools (like Alfred)
  (define-key global-map (kbd "s-v") 'yank)

  (add-hook 'after-make-frame-functions 'sulami/setup-frame)

  ;; Custom session save directory
  (defun emacs-session-filename (session-id)
    "Construct a filename to save the session in based on SESSION-ID. Customized
    version that saves to /tmp."
    (let ((basename (concat "session." session-id)))
      (concat "/tmp/.emacs-" basename)))

  ;; Fix evil keybindings in various modes
  (evil-collection-init)

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
  (add-hook 'smartparens-enabled-hook 'evil-cleverparens-mode)

  ;; Init modeline
  (sulami/init-modeline)

  ;; Proper in-/decrease
  (define-key evil-normal-state-map (kbd "C-1")
    'evil-numbers/dec-at-pt)
  (define-key evil-normal-state-map (kbd "C-2")
    'evil-numbers/inc-at-pt)

  ;; Trigger jump to word
  (define-key evil-normal-state-map (kbd "s-n") 'avy-goto-word-or-subword-1)

  ;; De-prettify org-mode
  ;; (add-hook 'org-mode-hook 'sulami/org-mode-format)

  ;; Enable so-long mode
  (when (require 'so-long nil :noerror)
    (so-long-enable))

  ;; Enable org-indent-mode
  (add-hook 'org-mode-hook 'org-indent-mode)

  ;; Disable flycheck on-the-fly checking for performance in large Python files
  (add-hook 'python-mode-hook (lambda () (sulami/flycheck-disable-for-large-files 2000)))

  ;; Clear highlight with return
  (define-key evil-normal-state-map (kbd "RET") 'sulami/isearch-nohighlight)

  ;; If inside a project, pop shells in the project root
  (define-key global-map (kbd "s-'") 'sulami/project-root-shell)

  ;; Terminals live in permanent holy mode
  (evil-set-initial-state 'term-mode 'emacs)

  (when (require 'term nil t)           ; only if term can be loaded..
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
  (define-key company-active-map (kbd "RET") 'company-complete-selection)

  ;; Fix C-w when autocompleting
  (define-key company-active-map (kbd "C-w") 'evil-delete-backward-word)

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

  ;; Ligature support, source: https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
  (mac-auto-operator-composition-mode)

  ;; Bring up the atomic-chrome editing server
  ;; (require 'atomic-chrome)
  ;; (atomic-chrome-start-server)
  ;; (setq atomic-chrome-default-major-mode 'markdown-mode
  ;;       atomic-chrome-buffer-open-style 'frame)

  (load-file custom-file)

  (setq file-name-handler-alist sulami/file-name-handler-alist)

  ;; Spacemacs-specific config
  (when (sulami/is-spacemacs)

    ;; Disable current line highlight
    (spacemacs/toggle-highlight-current-line-globally-off)

    ;; Ask before killing everything
    (spacemacs/set-leader-keys "q q" 'spacemacs/save-buffers-kill-emacs)

    ;; Kill project and layout
    (spacemacs/set-leader-keys "pK" 'sulami/kill-project-layout)

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

    ;; Adjust the font size on the fly
    (define-key evil-normal-state-map (kbd "C-0")
      'spacemacs/reset-font-size)
    (define-key evil-normal-state-map (kbd "C--")
      'spacemacs/scale-down-font)
    (define-key evil-normal-state-map (kbd "C-=")
      'spacemacs/scale-up-font)

    ;; More convenient than C-x #
    (spacemacs/set-leader-keys "qw" 'server-edit)

    ;; Import markdown to org-mode
    ;; (spacemacs/set-leader-keys-for-major-mode 'org-mode "em" 'sulami/markdown-to-org-mode)

    ;; Shortcuts to run single Python tests
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "tc" 'sulami/python-copy-current-test)
    (spacemacs/set-leader-keys-for-major-mode 'python-mode "tr" 'sulami/python-run-current-test)

    ;; Shortcut to open the test buffer
    (spacemacs/set-leader-keys "bt" 'sulami/open-test-buffer)

    ;; Kill the scratch frame
    (spacemacs/set-leader-keys "qy" 'sulami/kill-scratch-frame)

    ;; Map snippets from insert mode
    (define-key evil-insert-state-map (kbd "C-y") 'spacemacs/helm-yas)

    ;; Fix magit blame
    (spacemacs/set-leader-keys "gb" 'magit-blame-addition)

    ;; Sprunge doesn't really work any more, needs to be replaced
    ;; (spacemacs/set-leader-keys "b S" 'sulami/sprunge-buffer)
    ))
