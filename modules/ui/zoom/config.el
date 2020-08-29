;;; ui/zoom/config.el -*- lexical-binding: t; -*-

(use-package! zoom
  :defer 5
  :config
  (zoom-mode t)
  (setq zoom-size '(0.618 . 0.618)))
