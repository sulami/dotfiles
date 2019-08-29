;; Don't use package.el
(setq package-enable-at-startup nil)

;; Bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install & load up use-package
;; This is over here because it shaves a solid second off the startup time if I
;; compile it in.
(straight-use-package 'use-package)
(eval-when-compile
  (require 'use-package))

;; Eval the actual config
(require 'org-install)
(require 'ob-tangle)
(mapc #'org-babel-load-file (directory-files "/Users/sulami/.emacs" t "\\.org$"))
