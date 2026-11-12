;;; 32-woman.el --- Woman  -*- lexical-binding: t; -*-
;; man and woman are manual page readers for emacs. man just calls
;; the system man and shows the results in a buffer, while woman is a
;; full man page parser in Emacs Lisp. woman is much faster, but does
;; not support all formats of man pages. To solve this issue, I'm
;; patching woman to fall back to man if it fails to render, which is
;; still fast because it doesn't require man to build a man page index,
;; as we just pass it the correct file already.
(use-package man
  :straight nil
  :after (woman)
  :defer t
  :commands (woman)
  :config/el-patch
  (defun woman (&optional topic re-cache)
  "Browse UN*X man page for TOPIC (Without using external Man program).
The major browsing mode used is essentially the standard Man mode.
Choose the filename for the man page using completion, based on the
topic selected from the directories specified in `woman-manpath' and
`woman-path'.  The directory expansions and topics are cached for
speed.  With a prefix argument, force the caches to be
updated (e.g. to re-interpret the current directory).

Used non-interactively, arguments are optional: if given then TOPIC
should be a topic string and non-nil RE-CACHE forces re-caching."
    (interactive (list nil current-prefix-arg))
    ;; The following test is for non-interactive calls via gnudoit etc.
    (if (or (not (stringp topic)) (string-match-p "\\S " topic))
        (let ((file-name (woman-file-name topic re-cache)))
          (if file-name
              (el-patch-swap
                (woman-find-file file-name)
                (condition-case nil
                    (woman-find-file file-name)
                  (error (progn
                           (message "WoMan failed to format %s, falling back to `man'..." file-name)
                           (kill-buffer (alist-get file-name woman-buffer-alist))
                           (pop-to-buffer (man file-name))))))
            (message
             "WoMan Error: No matching manual files found in search path")
            (ding)))
      (message "WoMan Error: No topic specified in non-interactive call")
      (ding))))
