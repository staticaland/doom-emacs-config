;;; init.el -*- lexical-binding: t; -*-

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a "Module Index" link where you'll find
;;      a comprehensive list of Doom's modules and what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input

       :completion
       company           ; the ultimate code completion backend
       ivy               ; a search engine for love and life

       :ui
       zoom
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       indent-guides     ; highlighted indent columns
       modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink cursor line after big motions
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       treemacs          ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       workspaces        ; tab emulation, persistence & separate workspaces
       zen               ; distraction-free coding or writing

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       fold              ; (nigh) universal code folding
       (format +onsave)  ; automated prettiness
       reformatter
       snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

       :emacs
       (dired +icons)    ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer         ; interactive buffer management
       undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :checkers
       syntax              ; tasing you for every semicolon you forget

       :tools
       ansible
       docker
       editorconfig      ; let someone else argue about tabs vs spaces
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       lsp
       magit             ; a git porcelain for Emacs
       terraform         ; infrastructure as code
       tmux              ; an API for interacting with tmux

       :lang
       data              ; config/data formats
       emacs-lisp        ; drown in parentheses
       (go +lsp)         ; the hipster dialect
       json              ; At least it ain't XML
       markdown          ; writing docs for people to ignore
       (org +hugo +roam)
       (python +poetry +lsp)
       sh                ; she sells {ba,z,fi}sh shells on the C xor
       yaml              ; JSON, but readable

       :email

       :app
       (rss +org)        ; emacs as an RSS reader

       :config
       (default +bindings +smartparens))
