;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! dired-narrow :pin "f49a8bbf95f70671a74a24f7f4de453b2686be46")
(package! dired-open :pin "f49a8bbf95f70671a74a24f7f4de453b2686be46")
(package! keycast :pin "038475c178e90c7bad64d113db26d42cad60e149")
(package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5")
(package! modus-operandi-theme :pin "29c21b4a8146ae5d43cb3418480d307616dc2d8b")
(package! modus-vivendi-theme :pin "29c21b4a8146ae5d43cb3418480d307616dc2d8b")
(package! bicycle :pin "799969a66192b27c6464fc2e0025f4089d70493b")
(package! outline-minor-faces :pin "cb9c529bb992c6f60b054caf4e993b03c7b3ba9e")
(package! backline :pin "dc541a6daf82ab73774904ae9ccecd13e3c2af48")

(package! orgcss
  :recipe (:host github :repo "gongzhitaao/orgcss"
           :files ("src/css/*.css"))
  :pin "e7d24040fe6715bfdd5965f9acbc393fe612cef5")

(package! org-html-themes
  :recipe (:host github :repo "fniessen/org-html-themes"
           :files ("styles" "setup"))
  :pin "7e4e2ec56069dae3b40e2afdc0174bec368609d8")

(package! pretty-outlines
  :recipe (:host github :repo "ekaschalk/.spacemacs.d"
           :files ("layers/display/local/pretty-outlines/pretty-outlines.el"))
  :pin "ae7b9315fe6488cc84701ded0c9d601f9cb2a587")

(package! outshine :pin "9334b555aaf1426a9e405a57b80809a1615627b3")

(package! doct :pin "0a91f6b7fbb2e7971a4624a74ac74123cc514a4b")
(package! olivetti :pin "1a670ff64f66cb69f9e1520c009419cf09f47f58")
