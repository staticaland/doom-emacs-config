;; -*- no-byte-compile: t; -*-
;;; tools/org-roam-ui/packages.el

(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
