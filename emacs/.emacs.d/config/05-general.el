;;; 05-general.el --- General  -*- lexical-binding: t; -*-
;; General allows me to use fancy prefix keybindings.
;; I'm using a spacemacs-inspired system of a global leader key and a local leader
;; key for major modes. Bindings are setup in the respective use-package
;; declarations.
(use-package general
  :config
  (general-auto-unbind-keys)
  (general-evil-setup)
  (defconst leader-key "SPC")
  (general-create-definer leader-def
    :prefix leader-key
    :keymaps 'override
    :states '(normal visual))
  (defconst local-leader-key ",")
  (general-create-definer local-leader-def
    :prefix local-leader-key
    :keymaps 'override
    :states '(normal visual))
  (leader-def
    "" '(nil :wk "my lieutenant general prefix")
    ;; Prefixes
    "a" '(:ignore t :wk "app")
    "b" '(:ignore t :wk "buffer")
    "d" '(:ignore t :wk "dired")
    "f" '(:ignore t :wk "file")
    "f e" '(:ignore t :wk "emacs")
    "g" '(:ignore t :wk "git")
    "h" '(:ignore t :wk "help")
    "j" '(:ignore t :wk "jump")
    "k" '(:ignore t :wk "lisp")
    "l" '(:ignore t :wk "llm")
    "m" '(:ignore t :wk "mail")
    "o" '(:ignore t :wk "org")
    "p" '(:ignore t :wk "project/perspective")
    "s" '(:ignore t :wk "search/spell")
    "t" '(:ignore t :wk "toggle")
    "w" '(:ignore t :wk "window")
    ;; General keybinds
    "\\" 'indent-region
    "|" 'sulami/shell-command-on-region
    "a c" 'calc
    "a s" 'shell
    "b e" 'erase-buffer
    "b d" 'kill-current-buffer
    "b D" 'kill-buffer-and-window
    "b m" 'sulami/open-message-buffer
    "b ." 'sulami/open-minibuffer
    "b r" 'sulami/rename-file-and-buffer
    "b s" 'sulami/open-scratch-buffer
    "b y" 'sulami/copy-buffer
    "d d" #'dired
    "d s" #'sulami/open-source-dir
    "f f" 'find-file
    "f e e" 'sulami/open-emacs-config
    "f e r" 'sulami/reload-emacs-config
    "f D" 'sulami/delete-file-and-buffer
    "f R" 'sulami/rename-file-and-buffer
    "h e" 'info-display-manual
    "h g" 'general-describe-keybindings
    "h l" 'view-lossage
    "h m" 'woman
    "h v" 'describe-variable
    "t a" 'auto-fill-mode
    "t l" 'toggle-truncate-lines
    "t r" 'refill-mode
    "t s" 'flyspell-mode
    "t n" 'sulami/toggle-line-numbers
    "t N" 'sulami/toggle-narrow
    "t w" 'whitespace-mode
    "w =" 'balance-windows
    "w f" 'make-frame
    "w m" 'sulami/toggle-maximise-window
    "w u" 'winner-undo)
  (general-define-key
   "s-m" #'suspend-frame
   "s-t" #'sulami/toggle-term
   "s-u" #'universal-argument
   "s-=" (lambda () (interactive) (text-scale-increase 0.5))
   "s--" (lambda () (interactive) (text-scale-decrease 0.5))
   "s-0" (lambda () (interactive) (text-scale-increase 0))
   "M-q" #'sulami/fill-or-unfill)
  (general-nmap "g r" #'xref-find-references)
  ;; Dired
  (general-define-key
   :keymaps 'dired-mode-map
   "<return>" 'dired-find-alternate-file))
