;; These are taken from
;; https://github.com/raxod502/selectrum/wiki/Useful-Commands#isearch

;; Actions

(cl-defmacro selectrum-make-action ((&rest args) &body body)
  (declare (indent 1))
  `(lambda ()
     (interactive)
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda (,@args)
                    (put 'quit 'error-message "Quit")
                    (with-demoted-errors "Error: %S"
                      ,@body))
                  ,@(seq-take
                     `((if selectrum--refined-candidates (nth selectrum--current-candidate-index selectrum--refined-candidates))
                       selectrum--refined-candidates
                       (selectrum-get-current-input)
                       selectrum--current-candidate-index)
                     (length args)))
     (abort-recursive-edit)))

;; Swiper

(defvar selectrum-swiper-history nil "Submission history for `selectrum-swiper'.")
(autoload 'selectrum-read "selectrum")
(defun selectrum-swiper ()
  "Search for a matching line and jump to the beginning of its text.
The default candidate is a non-empty line closest to point.
This command obeys narrowing."
  (interactive)
  (let ((selectrum-should-sort-p nil)
        ;; Get the current line number for determining the travel distance.
        (current-line-number (line-number-at-pos (point) t)))
    (cl-destructuring-bind (default-candidate formatted-candidates)
        (cl-loop
              with buffer-lines = (split-string (buffer-string) "\n")
              with number-format = (concat "L%0"
                                           (number-to-string
                                            (length (number-to-string
                                                     (length buffer-lines))))
                                           "d: ")

              with formatted-candidates = nil
              for line-text in buffer-lines
              for line-num = (line-number-at-pos (point-min) t) then (1+ line-num)

              with default-candidate = nil
              with prev-distance-to-default-cand = 1.0e+INF ; This updated later.
              for distance-to-default-cand = (abs (- current-line-number line-num))

              unless (string-empty-p line-text) ; Just skip empty lines.
              do
              ;; Find if we’ve started to move away from the current line.
              (when (null default-candidate)
                (when (> distance-to-default-cand
                         prev-distance-to-default-cand)
                  (setq default-candidate (cl-first formatted-candidates)))
                (setq prev-distance-to-default-cand distance-to-default-cand))

              ;; Format current line and collect candidate.
              (push (propertize line-text
                                'selectrum-candidate-display-prefix
                                (propertize (format number-format line-num)
                                            'face 'completions-annotations)
                                'line-num line-num)
                    formatted-candidates)

              finally return (list default-candidate
                                   (nreverse formatted-candidates)))
      (let ((chosen-line-number
             (get-text-property
              0 'line-num
              (selectrum-read "Jump to matching line: "
                              formatted-candidates
                              :default-candidate default-candidate
                              :history 'selectrum-swiper-history
                              :require-match t
                              :no-move-default-candidate t))))
        (push-mark (point) t)
        (forward-line (- chosen-line-number current-line-number))
        (beginning-of-line-text 1)))))

(defun org:show-subtree-headlines ()
  "Show headlines surrounding point."
  (save-excursion
    (let ((points nil) (count 0))
      (unless (org-at-heading-p) (org-back-to-heading t))
      (push (point) points)
      (while (org-up-heading-safe)
        (push (point) points))
      (dolist (point points)
        (goto-char point)
        (when (org:heading-folded-p)
          (outline-toggle-children))))))

(defun selectrum:reveal-if-in-org-folds (orig-fn &rest args)
  (prog1 (apply orig-fn args)
    (when (eq major-mode 'org-mode)
      (org:show-subtree-headlines))))

(advice-add #'selectrum-swiper :around #'selectrum:reveal-if-in-org-folds)

;; Outline

(autoload 'selectrum-read "selectrum")
(defvar selectrum-outline-history nil
  "History of chosen headings for `selectrum-outline'.")

(defcustom selectrum-outline-formats
  ;; Groups: (1) level determinant, (2) heading text.
  ;; The top level is 0, for a zero-length determinant.
  '((emacs-lisp-mode
     . "^;;;\\(?1:;*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    (markdown-mode
     . "^#\\(?1:#*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    (outline-mode
     . "^\\*\\(?1:\\**\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    ;; For Org, see also `org-goto'.
    (org-mode
     . "^\\*\\(?1:\\**\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'")
    (python-mode
     . "^##\\(?1:\\**\\|#*\\)[[:blank:]]*\\(?2:[[:alnum:]][^z-a]*\\)\\'"))
  "An alist of regexps to use for identifying outline headings, one for each major mode.

The `car' of an item in the list should be a symbol of the major mode.
The `cdr' should be a regular expression with two required match groups:
1. Match group 1, whose length determines the outline level of that heading.
   For best formatting, the top level should be level 0 for zero length.
2. Match group 2, which is the actual heading text.

A heading is assumed to be on only one line. "
  :group 'selectrum
  :type '(alist
          :key-type (symbol :tag "Major mode symbol")
          :value-type (string :tag "Regexp")))

;;;###autoload
(defun selectrum-outline ()
  "Jump to a heading.  Regexps are pre-defined.  Obeys narrowing."
  (interactive)
  ;; Signal a `user-error' if we don't have a regexp for this major mode.
  (if-let ((heading-regexp (alist-get major-mode selectrum-outline-formats)))
      (let ((selectrum-should-sort-p nil) ; Headings should stay in order of appearance.
            ;; Get the basic information of each heading in the accessible
            ;; portion of the buffer.
            (buffer-lines (split-string (buffer-string) "\n"))
            (line-number 0)
            (line-number-format)

            ;; Finding the default heading
            (default-heading)
            (current-line-number (line-number-at-pos (point)))

            ;; Keeping track of the tree.
            (backwards-prefix-list)
            (prev-heading-text)
            (prev-heading-level)

            ;; Backwards result of the `dolist'. Will `nreverse'.
            (formatted-headings))

        (setq line-number-format
              (concat "L%0"
                      (number-to-string
                       (length (number-to-string (length buffer-lines))))
                      "d: "))

        (save-match-data
          (dolist (text-line buffer-lines)
            ;; Increment line number when moving to next.
            (cl-incf line-number)
            (when (string-match heading-regexp text-line)
              (let ((heading-text (match-string-no-properties 2 text-line))
                    (heading-level
                     (length (match-string-no-properties 1 text-line)))
                    (formatted-heading))

                ;; Want to make sure this has a correct value.
                (when (null prev-heading-level)
                  (setq prev-heading-level heading-level))

                ;; Decide whether to update the prefix list and the previous
                ;; heading level.
                (cond
                  ;; If we've moved to a greater level (further down the tree),
                  ;; add the previous heading to the heading prefix list so
                  ;; that we can prepend it to the current heading when
                  ;; formatting.
                  ((> heading-level prev-heading-level)
                   (setq backwards-prefix-list (cons prev-heading-text
                                                     backwards-prefix-list)
                         prev-heading-level heading-level))
                  ;; Otherwise, if we've moved to a lower level (higher up the
                  ;; tree), and need to remove the most recently added prefix
                  ;; from the list (i.e., go from '(c b a) back to '(b a)).
                  ((< heading-level prev-heading-level)
                   (setq backwards-prefix-list (last backwards-prefix-list
                                                     heading-level)
                         prev-heading-level heading-level))
                  ;; Otherwise, do nothing.
                  (t nil))

                ;; Regardless of what happens, update the previous heading text.
                (setq prev-heading-text heading-text)

                ;; Decide whether the previous formatted heading was the
                ;; default.
                (when (and (null default-heading)
                           (> line-number current-line-number))
                  (setq default-heading (car formatted-headings)))

                ;; Finally, add to list of formatted headings.
                ;; Create heading of form "L#: a/b/c" as:
                ;; - having a text property holding the line number
                ;; - prepended with a formatted line number,
                ;;   with the face `completions-annotations'.
                (push (propertize
                       (concat (string-join (reverse backwards-prefix-list) "/")
                               (and backwards-prefix-list "/")
                               heading-text)
                       'line-number line-number
                       'selectrum-candidate-display-prefix
                       (propertize
                        (format line-number-format line-number)
                        'face 'completions-annotations))
                      formatted-headings)))))

        ;; Now that candidates formatted, select from candidates.
        (let ((chosen-heading
               (selectrum-read "Jump to heading: "
                               (nreverse formatted-headings)
                               :default-candidate default-heading
                               :history 'selectrum-outline-history
                               :require-match t
                               :no-move-default-candidate t)))
          ;; Push mark, in case we want to return to current location.  This
          ;; needs to happen /after/ the user has made it clear that they want
          ;; to go somewhere.
          (push-mark (point) t)
          ;; Move to beginning of chosen line.
          (forward-line (- (get-text-property 0 'line-number chosen-heading)
                           current-line-number))
          (beginning-of-line-text 1)))
    (user-error "selectrum-outline: No headings defined for %s." major-mode)))

;; Project Search

(defvar selectrum--toggle-project-data+ nil)

(push (cons "C-," 'selectrum-toggle-project-file-scope+)
      selectrum-minibuffer-bindings)

(defun selectrum-toggle-project-file-scope+ ()
  "Toggle to project scope when reading file names.
Depends on `projectile'."
  (interactive)
  (unless minibuffer-completing-file-name
    (user-error "Not reading file names"))
  (require 'projectile)
  (setq selectrum--previous-input-string nil)
  (cond ((and selectrum--toggle-project-data+
              (string-match "in project: \\'"
                            (buffer-substring
                             (point-min) (minibuffer-prompt-end))))
         (let ((inhibit-read-only t))
           (save-excursion
             (goto-char (minibuffer-prompt-end))
             (search-backward " in project")
             (delete-region (match-beginning 0)
                            (match-end 0)))
           (delete-minibuffer-contents))
         (insert (car selectrum--toggle-project-data+))
         (setq selectrum--preprocessed-candidates
               (cdr selectrum--toggle-project-data+))
         (setq selectrum--toggle-project-data+ nil))
        (t
         (if-let ((input (selectrum-get-current-input))
                  (project (projectile-project-root
                            (file-name-directory input))))
             (let* ((inhibit-read-only t)
                    (ematch (file-name-nondirectory input))
                    (cands
                     (mapcar
                      (lambda (i)
                        (add-text-properties
                         0 (length i)
                         `(selectrum-candidate-full
                           ,(concat project i))
                         i)
                        i)
                      (projectile-project-files project))))
               (save-excursion
                 (goto-char (minibuffer-prompt-end))
                 (search-backward ":")
                 (insert
                  (apply #'propertize
                         " in project"
                         (text-properties-at (point)))))
               (setq selectrum--toggle-project-data+
                     (cons
                      input
                      selectrum--preprocessed-candidates))
               (delete-minibuffer-contents)
               (insert
                (concat (abbreviate-file-name project) ematch))
               (setq selectrum--preprocessed-candidates
                     (lambda (input)
                       (let ((ematch (file-name-nondirectory input)))
                         `((input . ,ematch)
                           (candidates . ,cands))))))
           (user-error "Not in project")))))

;; Imenu

(defvar selectrum-imenu+ nil)

(defun im/imenu+ ()
  "Choose from `imenu' just like `counsel-imenu'."
  (interactive)
  (require 'imenu)
  (let* ((selectrum-should-sort-p nil)
         (candidates (let* ((imenu-auto-rescan t)
                            (items (imenu--make-index-alist t)))
                       ;; remove *Rescan*
                       (setq items (delete (assoc "*Rescan*" items) items))
                       ;; special mode
                       (when (eq major-mode 'emacs-lisp-mode)
                         (let ((fns (cl-remove-if #'listp items :key #'cdr)))
                           (if fns (setq items (nconc (cl-remove-if #'nlistp items :key #'cdr) `(("Functions" ,@fns)))))))
                       ;; refine
                       (cl-labels ((get-candidates (alist &optional prefix)
                                     (cl-mapcan
                                      (lambda (elm)
                                        (if (imenu--subalist-p elm)
                                            (get-candidates
                                             (cl-loop for (e . v) in (cdr elm)
                                                   collect (cons e (if (integerp v) (copy-marker v) v)))
                                             (concat prefix (if prefix ".") (car elm)))
                                          (let ((key (concat (if prefix (concat (propertize prefix 'face 'font-lock-keyword-face) ": "))
                                                             (car elm))))
                                            (list (cons key (cons key (if (overlayp (cdr elm)) (overlay-start (cdr elm)) (cdr elm))))))))
                                      alist)))
                         (setq items (get-candidates items)))
                       ;; sort
                       (cl-sort items #'string< :key #'car)))
         (cand (completing-read "Imenu: " (mapcar #'car candidates) nil t nil selectrum-imenu+)))
    (imenu (cdr (cl-find cand candidates :test #'string= :key #'car)))))

;; Ripgrep

(defvar selectrum-search-rg-history nil)

(defun im/search-rg+ ()
  "Search like 'counsel-rg'.

Default, search for current directory, if the input begin with 'p ' then
will search current project, if begin with 'o ' then will search org-directory.

'C-c C-o' to pop the rg.el's Occur view, make sure package `rg' is installed."
  (interactive)
  (unless (executable-find "rg")
    (user-error "ripgrep must be installed."))
  (let* (type
         input
         (dir default-directory)
         (word (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let* ((sym (symbol-at-point)) (symn (symbol-name sym)))
                   (if (and sym (> 50 (length symn) 3)) symn nil))))
         (command (if (memq system-type '(ms-dos windows-nt))
                      "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R> ."
                    "rg -M 240 --with-filename --no-heading --line-number --color never -S -e <R>"))
         (cands (lambda (in)
                  (let ((msg)
                        (prop (lambda (cs)
                                (mapcar (lambda (c)
                                          (when (string-match "\\`\\([^:]+\\):\\([^:]+\\):" c)
                                            (add-face-text-property (match-beginning 1) (match-end 1) 'compilation-info nil c)
                                            (add-face-text-property (match-beginning 2) (match-end 2) '(:underline t :inherit compilation-line-number) nil c))
                                          c)
                                        cs))))
                    (cond
                      ;; search current project
                      ((string-prefix-p "p " in)
                       (cond ((not (project-current))
                              (setq msg "This is not in a project."))
                             ((< (length in) 5)
                              (setq msg "Search in current project, input should more than 3."))
                             (t
                              (setq type 'project)
                              (setq dir (cdr (project-current)))
                              (setq in (cl-subseq in 2)))))
                      ;; search org-directory
                      ((string-prefix-p "o " in)
                       (cond ((not (file-exists-p org-directory))
                              (setq msg "Org Directory not exist?"))
                             ((< (length in) 5)
                              (setq msg "Search in org-directory, input should more than 3."))
                             (t
                              (setq type 'org)
                              (setq dir org-directory)
                              (setq in (cl-subseq in 2)))))
                      ;; search current directory
                      (t (if (< (length in) 3)
                             (setq msg "Input should more than 3."))
                         (setq type nil)
                         (setq dir default-directory)))
                    ;; take space in INPUT as .*?
                    ;; take m-space as [[:blank:]]
                    (setq input
                          (replace-regexp-in-string
                           " +" "[[:blank:]]"
                           (replace-regexp-in-string
                            "\\([^ ]\\) \\([^ ]\\)" "\\1.+?\\2"
                            (string-trim in))))
                    (if msg
                        (prog1 nil
                          (setq-local selectrum-refine-candidates-function
                                      (lambda (_ __) (list msg))))
                      (kill-local-variable 'selectrum-refine-candidates-function)
                      (let* ((default-directory dir)
                             (cs (split-string
                                  (shell-command-to-string (grep-expand-template command input)) "\n")))
                        `((candidates . ,(funcall prop cs))
                          (input . ,input)))))))
         (cand (let ((selectrum-should-sort-p nil)
                     (selectrum-minibuffer-bindings
                      (append
                       selectrum-minibuffer-bindings
                       `(("C-c C-o" . ,(selectrum-make-action (c)
                                         ;; use rg.el to show the results in Occur buffer
                                         (require 'rg)
                                         (require 'compile)
                                         ;; jump to current candidate in the *rg* buffer.
                                         ;; rg implemented with `compile', so I make it work like below.
                                         ;; let-bound method not working, unkown reason.
                                         (let ((old-compilation-finish-functions compilation-finish-functions))
                                           (setq compilation-finish-functions
                                                 (list
                                                  (lambda (_a _b)
                                                    (unwind-protect
                                                         (progn
                                                           (pop-to-buffer (current-buffer))
                                                           (when (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" c)
                                                             (let ((file-name (match-string-no-properties 1 c))
                                                                   (line-number (match-string-no-properties 2 c)))
                                                               (if rg-group-result
                                                                   (progn
                                                                     (re-search-forward (format "^File: %s" file-name) nil t)
                                                                     (re-search-forward (format "^ *%s" line-number) nil t)
                                                                     (re-search-forward input (point-at-eol) t))
                                                                 (re-search-forward (format "%s:%s:" file-name line-number) nil t)
                                                                 (re-search-forward input (point-at-eol) t)))))
                                                      (setq compilation-finish-functions old-compilation-finish-functions)))))
                                           ;; dispatch to rg.el search.
                                           (cond ((eq type 'project) (rg-project input "*"))
                                                 (t                  (rg input "*" dir))))))))))
                 (selectrum-read "rg: " cands
                                 :initial-input word
                                 :may-modify-candidates t
                                 :history 'selectrum-search-rg-history
                                 :require-match t))))
    (if (string-match "\\`\\(.*?\\):\\([0-9]+\\):\\(.*\\)\\'" cand)
        (let ((file-name (match-string-no-properties 1 cand))
              (line-number (match-string-no-properties 2 cand)))
          (xref-push-marker-stack) ; use M-, to go back!
          (find-file (expand-file-name file-name dir))
          (goto-char (point-min))
          (forward-line (1- (string-to-number line-number)))
          (re-search-forward input (point-at-eol) t)
          (recenter))
      (message "Bad candidate?"))))
