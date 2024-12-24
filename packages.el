;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! evil-cleverparens :pin "22aa03d0f50aa70ae08fbe8765a88f5020afa635")
(package! ztree :pin "f05677f9696e573c8c607e8876fb4a0cccbc491f")

(package! gptel :pin "0f173bace51c0f68d0dc3667a340355996b418f0")
(package! aider :recipe (:host github :repo "tninja/aider.el" :files ("*.el")) :pin "515f5cbb505e614f400c21f99bdbebd47609c659")

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")) :pin "b5878d6a8c741138b5efbf4fe1c594f3fd69dbdd")

(package! org-ai :pin "5adfde1bc7db9026747fbfae4c154eeac4ef8e59")

(package! outline-minor-faces :pin "4628613f3570b865b2c22b750ebd41443c1848c2")

(package! bicycle :pin "04c3e44eb10303b81c47c1d333df1fa23a224963")

(package! ox-gfm :pin "46faa67dbb3fb0cd7a76c3fe518f16e4195c22c7")
