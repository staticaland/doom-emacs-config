;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;;;; Dired

(package! dired-narrow :pin "f49a8bbf95f70671a74a24f7f4de453b2686be46")
(package! dired-open :pin "f49a8bbf95f70671a74a24f7f4de453b2686be46")

;;;; Themes and appearance

(package! modus-operandi-theme :pin "29c21b4a8146ae5d43cb3418480d307616dc2d8b")
(package! modus-vivendi-theme :pin "29c21b4a8146ae5d43cb3418480d307616dc2d8b")

(package! outline-minor-faces :pin "cb9c529bb992c6f60b054caf4e993b03c7b3ba9e")
(package! backline :pin "dc541a6daf82ab73774904ae9ccecd13e3c2af48")
(package! bicycle :pin "799969a66192b27c6464fc2e0025f4089d70493b")

(package! pretty-outlines
  :recipe (:host github :repo "ekaschalk/.spacemacs.d"
           :files ("layers/display/local/pretty-outlines/pretty-outlines.el"))
  :pin "ae7b9315fe6488cc84701ded0c9d601f9cb2a587")

(package! outshine :pin "9334b555aaf1426a9e405a57b80809a1615627b3")

(package! olivetti :pin "1a670ff64f66cb69f9e1520c009419cf09f47f58")

(package! focus :pin "5f3f20e7f22fb9fd7c48abce8bd38061d97e4bc0")

;;;; Tools

(package! keycast :pin "038475c178e90c7bad64d113db26d42cad60e149")

(package! visual-regexp :pin "3e3ed81a3cbadef1f1f4cb16f9112a58641d70ca")

(package! yasnippet-terraform
    :recipe (:host github :repo "staticaland/yasnippet-terraform"
		:files ("terraform-mode"))
    :pin "ca87c64214839d7dc006d2b5ef54f494ca890a05")

;;;; OS specific

(when (eq system-type 'darwin)
    (package! elfeed-web :pin "73023f1cfbd94ab52fa785a7661d96aaa5d3c862" :disable nil))

;;;; Disabled

(package! emoji-cheat-sheet-plus :pin "ffcc84d7060dfa000148e7f8be4fd6701593a74f" :disable t)

(package! ivy-emoji :pin "a1b7d32048278afd9b06536a8af96f533639d146" :disable t)

(package! orgcss
  :recipe (:host github :repo "gongzhitaao/orgcss"
           :files ("src/css/*.css"))
  :pin "e7d24040fe6715bfdd5965f9acbc393fe612cef5" :disable t)

(package! org-html-themes
  :recipe (:host github :repo "fniessen/org-html-themes"
           :files ("styles" "setup"))
  :pin "7e4e2ec56069dae3b40e2afdc0174bec368609d8" :disable t)

(package! info-colors :pin "47ee73cc19b1049eef32c9f3e264ea7ef2aaf8a5" :disable t)

(package! doct :pin "0a91f6b7fbb2e7971a4624a74ac74123cc514a4b" :disable t)
