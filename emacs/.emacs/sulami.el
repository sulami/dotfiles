;; GENERAL STUFF

(defun sulami/layout-triple-fib ()
  "Open one window on the left and stacked on the right."
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)
  (select-window (next-window))
  (split-window-vertically))

(defun sulami/pop-file (file-path)
  "Pop up a buffer with a file path."
  (popwin:popup-buffer (find-file-noselect file-path)
                       :position :bottom
                       :stick t))

(defun sulami/open-message-buffer ()
  "Open the message buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

(defun sulami/open-test-buffer ()
  "Open the test buffer."
  (interactive)
  (switch-to-buffer "*Test*"))

(defun sulami/sprunge-buffer ()
  "Send the current buffer content to sprunge.us and copy the URL to the
clipboard."
  (interactive)
  (shell-command-on-region
   (point-min) (point-max)
   "curl -sF 'sprunge=<-' http://sprunge.us"
   "*Sprunge*")
  (with-current-buffer "*Sprunge*"
    (message (concat "Sprunged to " (buffer-string)))
    (spacemacs/copy-whole-buffer-to-clipboard)))

(defun sulami/save-to-junk ()
  "Save the current buffer as a junk file."
  (interactive)
  (spacemacs/copy-whole-buffer-to-clipboard)
  (kill-buffer)
  (open-junk-file)
  (insert (current-kill 0))
  (save-buffer))

(defun sulami/kill-project-layout ()
  "Kill the current project and then the layout."
  (interactive)
  (projectile-kill-buffers)
  (spacemacs/layouts-ts-kill))

(defun sulami/init-modeline ()
  "Start up my modeline."
  (require 'doom-modeline)
  (doom-modeline-mode 1)
  (setq doom-modeline-buffer-file-name-style 'relative-to-project)
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-persp-name nil))

;; ORG

(defun sulami/org-mode-format ()
  "Stop the org-level headers from being fancy."
  (interactive)
  (auto-composition-mode 0)
  (dolist (face '(org-level-1
                  org-level-2
                  org-level-3
                  org-level-4
                  org-level-5
                  org-level-6
                  org-level-7
                  org-level-8))
    (set-face-attribute face nil :inherit :family :weight 'normal :height 1.0)))

(defun sulami/markdown-to-org-mode ()
  "Markdown -> org-mode link conversion."
  (interactive)
  (evil-ex "%s/\\[\\(.*\\)\\](\\(.*\\))/[[\\2][\\1]]/g"))

;; PYTHON

(defun sulami/python-get-current-test ()
  "Get the current test path for pytest."
  (interactive)

  (cl-flet
      ((get-test-name
        ()
        (let* ((reset-point (point))
               (def-start (search-backward "def "))
               (_ (forward-char (length "def ")))
               (name-start (point))
               (name-end (- (search-forward "(") 1))
               (rv (buffer-substring name-start name-end)))
          (goto-char reset-point)
          rv))

      (get-class-name
        ()
        (let ((reset-point (point))
              (def-start (search-backward "class " nil t)))
          (if def-start
              (progn
                (forward-char (length "class "))
                (let* ((name-start (point))
                       (name-end (- (search-forward "(") 1))
                       (rv (buffer-substring name-start name-end)))
                  (goto-char reset-point)
                  (concatenate 'string rv "::")))
            "")))

      (get-test-path
        ()
        (let* ((splitted-path (split-string buffer-file-name "/"))
               (magic-headoff (nthcdr 5 splitted-path)))
          (joined (mapconcat 'identity magic-headoff "/")))))

    (concatenate 'string (get-test-path) "::" (get-class-name) (get-test-name))))

(defun sulami/python-run-test (test-path)
  "Run a test inside Docker."
  (let ((temp-buffer-name "*Test*")
        (inhibit-read-only t))
    (get-buffer-create temp-buffer-name)
    (let* ((test-command (concatenate 'string
                                      (projectile-project-root)
                                      "test "
                                      test-path))
            (process (start-process-shell-command "Test"
                                                  temp-buffer-name
                                                  test-command)))
      (with-current-buffer temp-buffer-name
        (erase-buffer)
        (require 'shell)
        (shell-mode)
        (set-process-filter process 'comint-output-filter)))
    (let ((temp-buffer-window (get-buffer-window temp-buffer-name)))
      (if temp-buffer-window
          (select-window temp-buffer-window)
        (popwin:popup-buffer temp-buffer-name
                              :position :bottom)))))

(defun sulami/python-run-current-test ()
  "Run the current test inside Docker."
  (interactive)
  (sulami/python-run-test (sulami/python-get-current-test)))

(defun sulami/python-copy-current-test ()
  "Copy the current test path for pytest."
  (interactive)
  (kill-new (sulami/python-get-current-test)))

;; INTERNALS

(defun sulami/setup-frame (&optional frame)
  "Set some options depending on whether we're in GUI or terminal mode."
  (interactive)
  (set-frame-parameter frame 'menu-bar-lines
                       (if (display-graphic-p frame)
                           1 0)))

(defun sulami/isearch-nohighlight ()
  "Remove search highlights if not in the isearch minor mode."
  (interactive)
  (when (not isearch-mode)
    (evil-search-highlight-persist-remove-all)))

(defun sulami/buffer-line-count ()
  "Get the number of lines in the active buffer."
  (count-lines 1 (point-max)))

(defun sulami/flycheck-disable-for-large-files (limit)
  "Disable flycheck on-the-fly-checking if the line count exceeds LIMIT."
  (setq flycheck-check-syntax-automatically
        (if (> (sulami/buffer-line-count) limit)
            (delete 'idle-change flycheck-check-syntax-automatically)
          (add-to-list 'flycheck-check-syntax-automatically 'idle-change))))

(defun sulami/project-root-shell ()
  "Pop the default shell in the project root if inside a project, otherwise in
the default directory."
  (interactive)
  (if (projectile-project-p)
      (projectile-with-default-dir (projectile-project-root)
        (spacemacs/default-pop-shell))
    (spacemacs/default-pop-shell)))

(defun sulami/magit-status-same-window ()
  "Open the magit status in the current window."
  (interactive)
  (let ((magit-display-buffer-function
         (lambda (buffer)
           (display-buffer buffer '(display-buffer-same-window)))))
    (magit-status)))

(defun sulami/scratch-frame ()
  "Open empty scratch buffer in new frame.

To be called from the outside using `emacsclient -a '' -e
\"(sulami/scratch-frame)\"`."
  (switch-to-buffer-other-frame "*scratch*")
  (spacemacs/toggle-maximize-buffer)
  (if (< 0 (buffer-size))
      (spacemacs/safe-erase-buffer)))

(defun sulami/kill-scratch-frame ()
  "Copy the content of the current buffer, empty it and kill the frame."
  (interactive)
  (clipboard-kill-ring-save (point-min) (point-max))
  (erase-buffer)
  (spacemacs/frame-killer))
