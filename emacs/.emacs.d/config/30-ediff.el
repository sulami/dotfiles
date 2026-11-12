;;; 30-ediff.el --- Ediff  -*- lexical-binding: t; -*-
;; Just diffing. I don't have any strong opinions on it, ediff does the
;; job. I'm sure there are cool features I'm not aware of.
;; Ignore whitespace changes
(setq ediff-diff-options "-w")
;; Don't create a new frame for the control window
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
;; Split horizontally by default
(setq ediff-split-window-function 'split-window-horizontally)
;; Pick both sides
;; Stolen from Stig.
(defun sulami/ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents
                     ediff-current-difference 'B
                     ediff-control-buffer))))

(defun sulami/add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map "d" #'sulami/ediff-copy-both-to-C))

(add-hook 'ediff-keymap-setup-hook #'sulami/add-d-to-ediff-mode-map)
