;;; tools/org-roam-ui/config.el -*- lexical-binding: t; -*-

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :commands org-roam-ui-mode
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))
