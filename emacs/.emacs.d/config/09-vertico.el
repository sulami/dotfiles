;;; 09-vertico.el --- Vertico  -*- lexical-binding: t; -*-
;; Vertico, the successor to Selectrum, is a great narrowing and
;; selection tool, intended to replace Ivy & Helm.
(use-package vertico
  :custom
  (completion-in-region-function #'consult-completion-in-region)
  :general
  (leader-def
    "b b" #'consult-buffer
    "p b" #'consult-project-buffer)
  :hook
  (after-init . vertico-mode))
;; Marginalia
(use-package marginalia
  :hook
  (vertico-mode . marginalia-mode))
;; Consult
;; Consult is counsel for vertico, in that it adds vertico support
;; for various commands that do not use read-string normally.
;; consult-flycheck cannot be autoloaded via consult, so it needs to
;; be loaded separately.
(use-package consult
  :defer t
  :straight
  '(consult
    :type git
    :host github
    :repo "minad/consult"
    :branch "main")
  :custom
  (consult-preview-key nil)
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  :config
  ;; Eshell only defines its locally keymap when you launch it, so we
  ;; have to add bindings with a hook.
  (defun sulami/consult-setup-eshell-bindings ()
    (general-imap
      :keymaps 'eshell-mode-map
      "C-r" 'consult-history))
  :general
  (leader-def
    "f r" 'consult-recent-file
    "j i" 'consult-imenu
    "j I" 'consult-project-imenu
    "j o" 'consult-outline
    "s s" 'consult-line)
  (general-imap
    "M-y" 'consult-yank-pop)
  (general-nmap
    "M-y" 'consult-yank-pop)
  (general-imap
    :keymaps 'shell-mode-map
    "C-r" 'consult-history)
  :hook
  ((eshell-mode . sulami/consult-setup-eshell-bindings)))

(use-package consult-flycheck
  :general
  (leader-def
    "j e" 'consult-flycheck))
;; Orderless
;; Orderless provides a minibuffer completion style that is "whitespace
;; separated words in any order." Very useful if you don't know exactly
;; what you're looking for.
(use-package orderless
  :defer t
  :custom
  (completion-styles '(orderless basic)))
;; Flyspell-correct
(use-package flyspell-correct
  :defer t
  :general
  (leader-def
    "s c" 'flyspell-correct-at-point))
