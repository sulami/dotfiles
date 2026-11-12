;;; 21-projectile.el --- Projectile  -*- lexical-binding: t; -*-
;; Manages projects (usually git repositories, but flexible).
(use-package projectile
  :custom
  (projectile-switch-project-action 'projectile-find-file)
  (projectile-completion-system 'default)
  :config
  (defun sulami/projectile-rg ()
    (interactive)
    (let ((vertico-count 30))
      (consult-ripgrep (projectile-project-root))))

  (defun sulami/projectile-rg-thing-at-point ()
    (interactive)
    (let ((vertico-count 30))
      (consult-ripgrep (projectile-project-root)
                       (thing-at-point 'symbol t))))

  (defun sulami/toggle-project-root-eshell ()
    "Opens eshell, if possible in the project root."
    (interactive)
    (cond
     ((eq major-mode 'eshell-mode)
      (evil-switch-to-windows-last-buffer))
     ((projectile-project-p (f-dirname (or (buffer-file-name)
                                           "")))
      (call-interactively #'projectile-run-eshell))
     ((eshell))))

  (defun sulami/toggle-project-root-shell ()
    "Opens shell, if possible in the project root."
    (interactive)
    (cond
     ((eq major-mode 'shell-mode)
      (evil-switch-to-windows-last-buffer))
     ((projectile-project-p (f-dirname (or (buffer-file-name)
                                           "")))
      (call-interactively #'projectile-run-shell))
     ((shell))))

  (defun sulami/toggle-project-root-term ()
    "Opens vterm, if possible in the project root."
    (interactive)
    (cond
     ((eq major-mode 'vterm-mode)
      (evil-switch-to-windows-last-buffer))
     ((projectile-project-p (f-dirname (or (buffer-file-name)
                                           "")))
      (let ((default-directory (projectile-project-root))
            (buf-name (concat "*vterm " (projectile-project-name) "*")))
        (if (get-buffer buf-name)
            (switch-to-buffer buf-name)
          (vterm buf-name))))
     ((vterm))))

  ;; Don't do projectile stuff on remote files
  ;; from https://github.com/syl20bnr/spacemacs/issues/11381#issuecomment-481239700
  (advice-add 'projectile-project-root :around
              (lambda (orig &rest args)
                (unless (file-remote-p default-directory)
                  (apply orig args))))

  :general
  (leader-def
    "d p" #'projectile-dired
    "p c" #'projectile-compile-project
    "p p" #'projectile-switch-project
    "p f" #'projectile-find-file
    "p k" #'projectile-kill-buffers
    "p s" #'sulami/toggle-project-root-shell
    "p t" #'sulami/toggle-project-root-term
    "s p" #'sulami/projectile-rg
    "s P" #'sulami/projectile-rg-thing-at-point)
  ("s-'" #'sulami/toggle-project-root-eshell)
    :hook (after-init . projectile-mode))
