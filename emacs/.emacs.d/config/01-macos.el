;;; 01-macos.el --- macOS  -*- lexical-binding: t; -*-
;; Everything in here relates to macOS in some way.
;; Swap the modifier keys
;; The MacPorts build I'm using swaps the modifiers from what I'm used to, so I'm
;; swapping them back.
(setq mac-command-modifier 'super
      mac-option-modifier 'meta)
;; Fix paste
;; Especially Alfred likes to paste with ⌘-v, so that needs to work.
(define-key global-map (kbd "s-v") 'yank)
;; Maximise with ⌘-Return
(define-key global-map (kbd "<s-return>") 'toggle-frame-maximized)
;; Mac font panel
(define-key global-map (kbd "s-f") #'mac-font-panel-mode)
;; Fix frame focus
;; The MacPorts Emacs version I'm using has the peculiar behaviour that
;; requires menu-bar-mode to be enabled in order to focus the current
;; frame when switching workspaces.
;; Use `mac-font-panel-mode' as a proxy to find out if this is the
;; MacPorts version.
(when (fboundp 'mac-font-panel-mode)
  (menu-bar-mode 1))
;; Get the macOS theme
(defun sulami/macos-dark-theme-p ()
  "Return non-nil if on macOS and currently in dark theme."
  (when (fboundp 'mac-application-state)
    (equal "NSAppearanceNameDarkAqua"
           (plist-get (mac-application-state) :appearance))))
