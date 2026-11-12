;;; 33-helpful.el --- Helpful  -*- lexical-binding: t; -*-
;; Helpful provides better Help buffers, with niceties such as "where
;; is this symbol referenced", and the full source code.
(use-package helpful
  :commands (helpful-symbol helpful-key)
  :config/el-patch
  (defun helpful-symbol (symbol)
    "Show help for SYMBOL, a variable, function or macro.

See also `helpful-callable' and `helpful-variable'."
    (interactive
     (list (helpful--read-symbol
            "Symbol: "
            (el-patch-wrap 2 1
              (condition-case nil
                  (helpful--symbol-at-point)
                (error nil)))
            #'helpful--bound-p)))
    (let ((c-var-sym (helpful--convert-c-name symbol t))
          (c-fn-sym (helpful--convert-c-name symbol nil)))
      (cond
       ((and (boundp symbol) (fboundp symbol))
        (if (y-or-n-p
             (format "%s is a both a variable and a callable, show variable?"
                     symbol))
            (helpful-variable symbol)
          (helpful-callable symbol)))
       ((fboundp symbol)
        (helpful-callable symbol))
       ((boundp symbol)
        (helpful-variable symbol))
       ((and c-fn-sym (fboundp c-fn-sym))
        (helpful-callable c-fn-sym))
       ((and c-var-sym (boundp c-var-sym))
        (helpful-variable c-var-sym))
       (t
        (user-error "Not bound: %S" symbol)))))

  :general
  (leader-def
    "h d" 'helpful-symbol
    "h f" 'helpful-function
    "h k" 'helpful-key))
