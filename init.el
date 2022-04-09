;;; init.el -*- lexical-binding: t; -*-


;;;; Input

(doom! :input

;;;; Completion

       :completion
       company
       vertico

;;;; UI

       :ui
       ;;zoom
       doom              ; what makes DOOM look the way it does
       ;; doom-dashboard    ; a nifty splash screen for Emacs
       ;; doom-quit         ; DOOM quit-message prompts when you quit Emacs
       ;; fill-column       ; a `fill-column' indicator
       ;; hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;indent-guides   ; highlighted indent columns
       modeline
       nav-flash         ; blink cursor line after big motions
       ;; ophints           ; highlight the region an operation acts on
       ;; (popup +defaults) ; tame sudden yet inevitable temporary windows
       treemacs          ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       workspaces        ; tab emulation, persistence & separate workspaces
       ;; zen               ; distraction-free coding or writing
       ;; deft

;;;; Editor

       :editor
       (evil +everywhere); come to the dark side, we have cookies
       ;; file-templates    ; auto-snippets for empty files
       ;;fold              ; (nigh) universal code folding
       ;;lispy
       ;; reformatter
       ;; snippets          ; my elves. They type so I don't have to
       word-wrap         ; soft wrapping with language-aware indent

;;;; Emacs

       :emacs
       (dired +icons)    ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       ibuffer         ; interactive buffer management
       ;; undo              ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

;;;; Checkers

       :checkers
       syntax              ; tasing you for every semicolon you forget

;;;; Tools

       :tools
       ;; ansible
       ;; docker
       editorconfig      ; let someone else argue about tabs vs spaces
       ;; (eval +overlay)     ; run code, run (also, repls)
       ;;lookup              ; navigate your code and its documentation
       lsp
       magit             ; a git porcelain for Emacs
       terraform         ; infrastructure as code
       ;; tmux              ; an API for interacting with tmux

;;;; Term

       :term
       ;; vterm

;;;; Lang

       :lang
       data              ; config/data formats
       emacs-lisp
       ;; (go +lsp)
       json              ; At least it ain't XML
       markdown          ; writing docs for people to ignore
       (org +pretty +journal +hugo +roam +present)
       (python +poetry +lsp +pyright)
       sh
       yaml              ; JSON, but readable

;;;; OS

       :os
       macos

;;;; Email

       :email

;;;; App

       :app
       ;; (rss +org)        ; emacs as an RSS reader

;;;; Config

       :config
       (default +bindings +smartparens)
       literate)
