;;; 36-email.el --- Email  -*- lexical-binding: t; -*-
;; This is my email setup. I fetch email into a local maildir, which is
;; available offline, and also much faster.
(setq message-directory "~/.mail"
      message-kill-buffer-on-exit t
      message-send-mail-function 'message-send-mail-with-sendmail
      message-sendmail-envelope-from 'header
      sendmail-program "msmtp"
      mail-specify-envelope-from t
      mail-envelope-from 'header)
