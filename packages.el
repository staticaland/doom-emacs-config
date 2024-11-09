;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

(package! ox-gfm :pin "46faa67dbb3fb0cd7a76c3fe518f16e4195c22c7")

(package! gptel :pin "0f173bace51c0f68d0dc3667a340355996b418f0")
(package! aider :recipe (:host github :repo "tninja/aider.el" :files ("*.el")) :pin "515f5cbb505e614f400c21f99bdbebd47609c659")

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")) :pin "b5878d6a8c741138b5efbf4fe1c594f3fd69dbdd")
