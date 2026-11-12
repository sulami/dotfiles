;;; 35-erc.el --- ERC  -*- lexical-binding: t; -*-
;; IRC. I don't use IRC a lot, but every now and then. As such I like ERC
;; because it works reasonably well out of the box, and I don't need to
;; yak shave an awful lot.
(use-package erc
  :straight nil
  :defer t
  :commands (erc sulami/erc)
  :custom
  (erc-nick "sulami")
  (erc-join-buffer 'bury)
  (erc-hide-list '("JOIN" "PART" "QUIT"))
  (erc-lurker-hide-list '("JOIN" "PART" "QUIT"))
  (erc-rename-buffers t)
  (erc-interpret-mirc-color t)
  (erc-timestamp-only-if-changed-flag nil)
  (erc-timestamp-format "%H:%M ")
  (erc-fill-prefix nil)
  (erc-fill-function 'erc-fill-variable)
  (erc-insert-timestamp-function 'erc-insert-timestamp-left)
  (erc-autojoin-channels-alist '(("irc.libera.chat" "#nixers_net")
                                 ("irc.circleci.com" "#general")))
  :config
  (add-to-list 'erc-modules 'keep-place)
  (add-to-list 'erc-modules 'spelling)
  (defun sulami/erc ()
    (interactive)
    (erc :server "irc.libera.chat"
         :port 6667
         :nick "sulami"
         :password (string-trim (shell-command-to-string "pass libera/password")))
    (erc :server "irc.circleci.com"
         :port 6667
         :nick "robins"
         :password (string-trim (shell-command-to-string "pass circleci/irc/password"))))
  :general
  (leader-def
    "a i" 'erc-track-switch-buffer))
