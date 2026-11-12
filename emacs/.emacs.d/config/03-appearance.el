;;; 03-appearance.el --- Appearance  -*- lexical-binding: t; -*-
;; Font
;; Set the font to Fira Code and enable ligatures.
(let ((font "PragmataPro Mono 14"))
  (set-face-attribute 'default nil :font font)
  (set-frame-font font nil t))
;; (when (boundp 'mac-auto-operator-composition-mode)
;;   (mac-auto-operator-composition-mode))
;; Theme
;; I use doom-themes for the dark theme and leuven for the light one.
;; There are some fixes to prevent themes from clashing, and I also
;; disable most backgrounds as I find them distracting.
;; I like to live dangerously
(setq custom-safe-themes t)

(defconst sulami/light-theme 'leuven)
(defconst sulami/dark-theme 'doom-gruvbox)

(defun sulami/disable-all-themes ()
  "Disables all custom themes."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(defun sulami/before-load-theme-advice (theme &optional no-confirm no-enable)
  "Disable all themes before loading a new one.

Prevents mixing of themes, where one theme doesn't override all faces
of another theme."
  (sulami/disable-all-themes))

(advice-add 'load-theme
            :before
            #'sulami/before-load-theme-advice)

(defun sulami/set-face-straight-underline (face)
  "Remove FACE's :underline style, keeping its colour.

The attribute is a plist such as (:style wave :color \"Red1\").  Read it
by key rather than by position: depending on which theme set it, :color
and :style appear in either order, and taking a fixed index picks up the
symbol `wave' for the colour, which `set-face-attribute' rejects with
\"Invalid face underline wave\"."
  (when (display-graphic-p)
    (let ((old-attr (face-attribute face :underline)))
      (when (consp old-attr)
        (let ((color (plist-get old-attr :color))
              (position (plist-get old-attr :position)))
          (set-face-attribute
           face nil :underline
           (cond ((and color position) (list :color color :position position))
                 (color color)
                 ((not position) t)
                 (t (list :position position)))))))))

(defconst sulami/org-theme-faces
  '(outline-1 org-block org-block-begin-line org-block-end-line org-quote)
  "Org faces whose background is cleared after loading a theme.")

(defun sulami/clear-org-face-backgrounds ()
  "Clear the background of org's faces, where they exist.

These faces are only defined once org is loaded, and a theme is applied
before org may be, so a missing face must not be an error."
  (dolist (face sulami/org-theme-faces)
    (when (facep face)
      (set-face-background face nil))))

(defun sulami/after-load-theme-advice (theme &optional no-confirm no-enable)
  "Unsets backgrounds for some org-mode faces.

Also changes squiggly underlines to straight ones.

Org and flycheck may not be loaded yet, so anything that would fail on
a missing face is guarded; the org work is redone from
`sulami/after-org-load-theme-advice' when org does load."
  (sulami/clear-org-face-backgrounds)
  (dolist (face '(flycheck-error flycheck-warning flycheck-info))
    (when (facep face)
      (sulami/set-face-straight-underline face))))

(defun sulami/after-org-load-theme-advice ()
  "Apply theme face tweaks that need org's faces to exist."
  (sulami/clear-org-face-backgrounds))

(advice-add 'load-theme
            :after
            #'sulami/after-load-theme-advice)

(with-eval-after-load 'org
  (sulami/after-org-load-theme-advice))

(use-package doom-themes
  :after (dash)
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (doom-themes-org-config)
  ;; Set the default colourscheme according to the time of day
  :hook
  (after-init . (lambda ()
                  (when (display-graphic-p)
                    (let* ((hour-of-day (read (format-time-string "%H")))
                           (theme (if (or (not (sulami/macos-dark-theme-p))
                                          (<= 8 hour-of-day 17))
                                      sulami/light-theme
                                    sulami/dark-theme)))
                      (load-theme theme t)
                      (sulami/after-load-theme-advice theme))))))

(use-package mac-auto-theme
  :straight nil
  :no-require t
  :if (fboundp 'mac-application-state)
  :after (doom-themes)
  :hook
  (mac-effective-appearance-change . (lambda ()
                                       (when (display-graphic-p)
                                         (load-theme
                                          (if (sulami/macos-dark-theme-p)
                                              sulami/dark-theme
                                            sulami/light-theme)
                                          t)))))
;; Modeline
;; I use doom-modeline, without any icons, and patched to be regular
;; height.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-height 10)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-buffer-encoding nil)
  (doom-modeline-persp-name nil)
  (doom-modeline-vcs-max-length 36)
  :config/el-patch
  (defun doom-modeline--font-height ()
    "Calculate the actual char height of the mode-line."
    (let ((height (face-attribute 'mode-line :height)))
      ;; WORKAROUND: Fix tall issue of 27 on Linux
      ;; @see https://github.com/seagle0128/doom-modeline/issues/271
      (round
       (* (if (and (>= emacs-major-version 27)
                   (not (eq system-type 'darwin)))
              1.0
            (if doom-modeline-icon
                (el-patch-swap 1.68 1.0)
              (el-patch-swap 1.25 1.0)))
          (cond ((integerp height) (/ height 10))
                ((floatp height) (* height (frame-char-height)))
                (t (frame-char-height))))))))
;; Terminal mode
;; Hide the TUI menu bar and enable mouse support in terminal mode.
(unless window-system
  (menu-bar-mode 0)
  (xterm-mouse-mode 1))
