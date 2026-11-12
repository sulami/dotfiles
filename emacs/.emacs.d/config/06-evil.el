;;; 06-evil.el --- Evil  -*- lexical-binding: t; -*-
;; This provides vim-style modal editing. There is quite a bit of
;; boilerplate to make it work with the various components, but I really
;; can't stand the default Emacs keybindings.
(use-package evil
  :init
  (setq evil-want-C-u-scroll t
        evil-want-C-i-jump t
        evil-want-Y-yank-to-eol t
        evil-want-keybinding nil
        evil-ex-visual-char-range t
        evil-move-beyond-eol t
        evil-disable-insert-state-bindings t)
  :custom
  (evil-undo-system 'undo-fu)
  :config
  ;; This conflicts with the local leader
  (unbind-key "," evil-motion-state-map)

  (defun sulami/evil-shift-left-visual ()
    "`evil-shift-left`, but keeps the selection."
    (interactive)
    (call-interactively 'evil-shift-left)
    (evil-normal-state)
    (evil-visual-restore))

  (defun sulami/evil-shift-right-visual ()
    "`evil-shift-right`, but keeps the selection."
    (interactive)
    (call-interactively 'evil-shift-right)
    (evil-normal-state)
    (evil-visual-restore))

  :general
  (leader-def
   "TAB" #'evil-switch-to-windows-last-buffer
   "<tab>" #'evil-switch-to-windows-last-buffer
   "w d" #'evil-window-delete
   "w h" #'evil-window-move-far-left
   "w j" #'evil-window-move-very-bottom
   "w k" #'evil-window-move-very-top
   "w l" #'evil-window-move-far-right
   "w /" #'evil-window-vsplit
   "w -" #'evil-window-split)
  (general-imap
    "C-w" #'evil-delete-backward-word
    "C-k" #'evil-insert-digraph)
  (general-vmap
    ">" #'sulami/evil-shift-right-visual
    "<" #'sulami/evil-shift-left-visual)
  :hook (after-init . evil-mode))
;; evil-collection
;; This adds evil-keybindings for a lot of popular modes.
;; I have to disable some because they clash with my own.
(use-package evil-collection
  :after (evil)
  :config
  (setq evil-collection-mode-list
        (->> evil-collection-mode-list
             (delete 'company)
             (delete 'gnus)
             (delete 'lispy)))
  (evil-collection-init))
;; evil-org
;; Evil-keybindings for org/agenda.
(use-package evil-org
  :after (org)
  :config
  (require 'evil-org-agenda)
  :hook ((org-mode . evil-org-mode)
         (org-agenda-mode . evil-org-agenda-set-keys)))
;; evil-commentary
;; vim-commentary but for evil.
(use-package evil-commentary
  :hook (evil-mode . evil-commentary-mode))
;; evil-surround
;; vim-surround but for evil.
(use-package evil-surround
  :hook (evil-mode . global-evil-surround-mode))
;; evil-numbers
(use-package evil-numbers
  :defer t
  :general
  (general-nvmap
    "C-a" 'evil-numbers/inc-at-pt
    "C-z" 'evil-numbers/dec-at-pt))
