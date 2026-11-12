;;; 04-functions.el --- Custom functions  -*- lexical-binding: t; -*-
;; Config
;; Open this file
(defun sulami/open-emacs-config ()
  "Opens the config files for our favourite OS."
  (interactive)
  (dired sulami/emacs-config-directory))
;; Reload this file
(defun sulami/reload-emacs-config ()
  "Loads the config file for our favourite OS."
  (interactive)
  (sulami/load-config-files))
;; Buffers
;; Rename buffer file
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
;; Switch to buffer shortcuts
(defun sulami/open-scratch-buffer ()
  "Opens the scratch buffer."
  (interactive)
  (switch-to-buffer "*scratch*"))

(defun sulami/open-message-buffer ()
  "Opens the message buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun sulami/open-minibuffer ()
  "Focusses the minibuffer, if active."
  (interactive)
  (when (active-minibuffer-window)
    (select-window (minibuffer-window))))
;; Buffer line count
(defun sulami/buffer-line-count ()
  "Get the number of lines in the active buffer."
  (count-lines 1 (point-max)))
;; Delete buffer file
(defun sulami/delete-file-and-buffer ()
  "Deletes a buffer and the file it's visiting."
  (interactive)
  (when-let* ((file-name (buffer-file-name))
              (really (yes-or-no-p (format "Delete %s? "
                                           file-name))))
    (delete-file file-name)
    (kill-buffer)))
;; Copy buffer
(defun sulami/copy-buffer ()
  "Copies the entire buffer to the kill-ring."
  (interactive)
  (copy-region-as-kill 1 (point-max)))
;; Open the source directory
(defun sulami/open-source-dir ()
  (interactive)
  (find-file sulami/source-directory))
;; Toggle a terminal
(defun sulami/toggle-term ()
  "Opens global vterm, or switches to last buffer."
  (interactive)
  (let ((buf-name "*vterm*"))
    (cond
     ((eq major-mode 'vterm-mode)
      (evil-switch-to-windows-last-buffer))
     ((get-buffer buf-name)
      (switch-to-buffer buf-name))
     ((vterm buf-name)))))
;; Windows
;; Maximise a window
(defun sulami/toggle-maximise-window ()
  "Toggles maximising the current window.

From: https://gist.github.com/mads-hartmann/3402786"
  (interactive)
  (if (and (= 1 (length (window-list)))
           (assoc ?_ register-alist))
      (jump-to-register ?_)
    (progn
      (window-configuration-to-register ?_)
      (delete-other-windows))))
;; Run a shell command on a region
(defun sulami/shell-command-on-region (beg end cmd)
  (interactive "r\nsCommand: ")
  (shell-command-on-region beg end cmd t t))
;; Sort words
(defun sulami/sort-words (beg end)
  "Sorts words in region."
  (interactive "r")
  (sort-regexp-fields nil "\\w+" "\\&" beg end))
;; Kill matching lines
(defun sulami/kill-matching-lines (regexp &optional rstart rend interactive)
  "Kill lines containing matches for REGEXP.

See `flush-lines' or `keep-lines' for behavior of this command.

If the buffer is read-only, Emacs will beep and refrain from deleting
the line, but put the line in the kill ring anyway.  This means that
you can use this command to copy text from a read-only buffer.
\(If the variable `kill-read-only-ok' is non-nil, then this won't
even beep.)"
  (interactive
   (keep-lines-read-args "Kill lines containing match for regexp"))
  (let ((buffer-file-name nil)) ;; HACK for `clone-buffer'
    (with-current-buffer (clone-buffer nil nil)
      (let ((inhibit-read-only t))
        (keep-lines regexp rstart rend interactive)
        (kill-region (or rstart (line-beginning-position))
                     (or rend (point-max))))
      (kill-buffer)))
  (unless (and buffer-read-only kill-read-only-ok)
    ;; Delete lines or make the "Buffer is read-only" error.
    (flush-lines regexp rstart rend interactive)))
;; Toggle narrowing
(defun sulami/toggle-narrow ()
  "Toggles `narrow-to-defun' or `org-narrow-to-subtree'."
  (interactive)
  (if (buffer-narrowed-p)
      (widen)
    (if (eq major-mode 'org-mode)
        (org-narrow-to-subtree)
      (narrow-to-defun))))
;; Toggle line numbers
;; This one is faster than linum-mode.
(defun sulami/toggle-line-numbers ()
  "Toggles buffer line number display."
  (interactive)
  (setq display-line-numbers (not display-line-numbers)))
;; Find the font face used
;; This one is quite useful for debugging syntax highlighting. It's
;; adapted from here.
(defun sulami/what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face
        (message "Face: %s" face)
      (message "No face at %d" pos))))
;; Create a random UUID
;; I need random UUIDs all the time. This generates one and places it in
;; the clipboard, ready for pasting. Heavily dependent on macOS.
(defun sulami/random-uuid ()
  (interactive)
  (let ((uuid (s-trim (shell-command-to-string "uuidgen | tr '[:upper:]' '[:lower:]'"))))
    (kill-new uuid)
    (message "Generated UUID: %s" uuid)))
;; Copy the path & line at point
;; This is useful to run Rspec tests.
(defun sulami/rspec-path ()
  (interactive)
  (let ((fp (f-relative buffer-file-name (projectile-project-root)))
        (ln (1+ (line-number-at-pos))))
    (-> (format "%s:%d" fp ln)
        (message)
        (kill-new))))
;; Fill/unfill paragraph
;; Fills the current paragraph. If used again, "unfills" it, for pasting
;; in places that doesn't like hard line breaks, such as GitHub. "Taken
;; from here.
(defun sulami/fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command #'sulami/fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (if (eq major-mode 'org-mode)
        (call-interactively #' org-fill-paragraph)
      (call-interactively #'fill-paragraph))))
