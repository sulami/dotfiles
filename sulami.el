;; GENERAL STUFF

(defun sulami/open-message-buffer ()
  "Open the message buffer."
  (interactive)
  (switch-to-buffer "*Messages*"))

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

;; PYTHON

(defun sulami/python-get-current-test ()
  "Get the current test path for pytest."
  (interactive)

  (cl-flet
      ((get-test-name
        ()
        (setq reset-point (point)
              def-start (search-backward "def "))
        (forward-char (length "def "))
        (setq name-start (point)
              name-end (- (search-forward "(") 1)
              rv (buffer-substring name-start name-end))
        (goto-char reset-point)
        rv)

      (get-class-name
        ()
        (setq reset-point (point)
              def-start (search-backward "class " nil t))
        (if def-start
            (progn
              (forward-char (length "class "))
              (setq name-start (point)
                    name-end (- (search-forward "(") 1)
                    rv (buffer-substring name-start name-end))
              (goto-char reset-point)
              (concatenate 'string rv "::"))
          ""))

      (get-test-path
        ()
        (setq splitted-path (split-string buffer-file-name "/")
              magic-headoff (nthcdr 5 splitted-path)
              joined (mapconcat 'identity magic-headoff "/"))))

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
