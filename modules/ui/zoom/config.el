;;; ui/zoom/config.el -*- lexical-binding: t; -*-

;; https://github.com/hlissner/doom-emacs/issues/2225
(use-package zoom
  :hook (doom-first-input . zoom-mode)
  :config
  (setq zoom-size '(0.618 . 0.618)
        zoom-ignored-major-modes '(dired-mode vterm-mode help-mode helpful-mode rxt-help-mode help-mode-menu org-mode)
        zoom-ignored-buffer-names '("*doom:scratch*" "*info*" "*helpful variable: argv*")
        zoom-ignored-buffer-name-regexps '("^\\*calc" "\\*helpful variable: .*\\*")
        zoom-ignore-predicates (list (lambda () (> (count-lines (point-min) (point-max)) 20)))))
