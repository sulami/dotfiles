;;; 27-eshell.el --- Eshell  -*- lexical-binding: t; -*-
;; Eshell is my main shell these days, mostly because it integrates so
;; well with Emacs. On rare occasions I use a terminal emulator (usually
;; also inside Emacs) with zsh.
;; Prompt
(setq eshell-prompt-function
      (lambda ()
        (concat
         (when (not (eshell-exit-success-p))
           (concat
            "<"
            (propertize (number-to-string eshell-last-command-status)
                        'face `(:foreground "red"))
            "> "))
         (abbreviate-file-name (eshell/pwd))
         " λ "))
      eshell-prompt-regexp
      (rx (opt "<" (1+ digit) "> ")
          (1+ anything)
          " λ "))
;; Aliases
;; This just loads my aliases. They are auto-generated from my zsh
;; aliases, so that the two are always in sync.
(setq eshell-aliases-file "~/.emacs.d/aliases")
;; Completion
;; Eshell doesn't do context-aware autocompletion by default and defaults
;; to completing filenames instead. Luckily we can easily define custom
;; completion handlers for commands.
;; Disable the completion buffer
;; This swaps the terrible popup buffer that eshell opens when I hit
;; TAB for a read-string completion.
;; The binding has to happen here in a hook because eshell-mode-map
;; isn't available before eshell is started.
(add-hook
 'eshell-mode-hook
 (lambda ()
   (setq completion-at-point-functions '(comint-completion-at-point t))
   (define-key eshell-mode-map (kbd "TAB") 'completion-at-point)
   (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)))
;; Sudo
(defun pcomplete/sudo ()
  "Completion rules for the `sudo' command."
  (let ((pcomplete-ignore-case t))
    (pcomplete-here (funcall pcomplete-command-completion-function))
    (while (pcomplete-here (pcomplete-entries)))))
;; Timestamps
;; I like having timestamps in my shell history, so that I can use the
;; history for logs later on.
(defun sulami/eshell-insert-timestamp ()
  (when eshell-mode
    (save-excursion
      (goto-char eshell-last-input-start)
      (let ((ts (format-time-string "[%FT%TZ] " nil t)))
        (insert ts)
        (put-text-property eshell-last-input-start
                           (+ eshell-last-input-start
                              (length ts))
                           'face
                           'eshell-prompt)))))

(add-hook 'eshell-pre-command-hook #'sulami/eshell-insert-timestamp)
