;;; 44-web.el --- Web  -*- lexical-binding: t; -*-
(use-package web-mode
  :defer t
  :custom
  (js-indent-level 2)
  (web-mode-code-indent-offset 2)
  (web-mode-markup-indent-offset 2)
  (web-mode-script-padding 0)
  (web-mode-style-padding 0)
  :mode
  (("\\.tsx" . web-mode)
   ("\\.vue" . web-mode)))
