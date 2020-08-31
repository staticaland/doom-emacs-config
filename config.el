;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Anders K. Pettersen"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'modus-operandi)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

(setq evil-snipe-scope 'visible)


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;



;;;; Config

(setq doom-variable-pitch-font (font-spec :family "EtBembo" :size 18))
(setq doom-themes-treemacs-enable-variable-pitch nil)

(setq +workspaces-on-switch-project-behavior t)

(after! which-key
  (setq which-key-idle-delay 0.5
        which-key-idle-secondary-delay 0.01
        which-key-sort-order 'which-key-key-order-alpha))

(setq org-babel-python-command "python3")

(when (string= (system-name) "Anderss-MacBook-Pro.local")
  (setq doom-font (font-spec :family "Menlo" :size 15)
        doom-big-font-increment 8
        doom-variable-pitch-font (font-spec :family "EtBembo" :size 22))
  (font-put doom-font :weight 'semi-light))

(use-package! treemacs
  :commands treemacs
  :init
    (map! :leader
      (:prefix ("f" . "file")
        :desc "Open Treemacs" "t" #'+treemacs/toggle))
  :config
    (treemacs-git-mode 'extended)
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

(after! dired
  (if (executable-find "gls")
      (progn
        (setq insert-directory-program "gls")
        (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
    (setq dired-listing-switches "-ahlF"))

  (setq ls-lisp-dirs-first t)

  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies (quote always))
  (setq dired-recursive-deletes (quote top)))

(use-package dired
  :commands dired
  :ensure nil
  :hook ((dired-mode . dired-hide-details-mode)
         (dired-mode . hl-line-mode))
  :config
  (setq delete-by-moving-to-trash t)
  (setq dired-listing-switches "-lat") ; sort by date (new first)
  (put 'dired-find-alternate-file 'disabled nil))

(use-package! dired-narrow
  :after dired
  :config
    (map! :map dired-mode-map
      :n  "/" 'dired-narrow-fuzzy))

(use-package! dired-open
  :after dired
  :config
  (setq open-extensions
      '(("webm" . "mpv")
        ("avi" . "mpv")
        ("mp3" . "mpv")
        ("mp4" . "mpv")
        ("m4a" . "mpv")
        ("mkv" . "mpv")
        ("ogv" . "mpv")
        ("pdf" . "zathura")))
    (setq dired-open-extensions open-extensions))

(use-package! osx-trash
  :after dired
  :if (eq system-type 'darwin)
  :init
  (osx-trash-setup))

(use-package! elfeed
  :commands elfeed
  :init
  (map! :leader
    (:prefix ("o" . "open")
      :desc "Open elfeed" "e" #'=rss)))

(use-package! emacs
  :init
    (map! :leader
      (:prefix ("ø" . "utils")
        :desc "tmux buffer" "t" #'const/tmux-capture-pane))
  :config
  (setq display-line-numbers-type nil)
  (defun const/tmux-capture-pane()
    (interactive)
    (with-output-to-temp-buffer "*tmux-capture-pane*"
      (shell-command "tmux capture-pane -p -S -"
                     "*tmux-capture-pane*"
                     "*Messages*")
	(pop-to-buffer "*tmux-capture-pane*"))))

(add-hook! 'org-mode-hook #'mixed-pitch-mode)

(setq evil-vsplit-window-right t
      evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-workspace-buffer))

(use-package! keycast
  :commands keycast-mode
  :config
  (define-minor-mode keycast-mode
    "Show current command and its key binding in the mode line."
    :global t
    (if keycast-mode
        (progn
          (add-hook 'pre-command-hook 'keycast-mode-line-update t)
          (add-to-list 'global-mode-string '("" mode-line-keycast " ")))
      (remove-hook 'pre-command-hook 'keycast-mode-line-update)
      (setq global-mode-string (remove '("" mode-line-keycast " ") global-mode-string))))
  (custom-set-faces!
    '(keycast-command :inherit doom-modeline-debug
                      :height 0.9)
    '(keycast-key :inherit custom-modified
                  :height 1.1
                  :weight bold)))

(use-package! info-colors
  :commands (info-colors-fontify-node))

(add-hook 'Info-selection-hook 'info-colors-fontify-node)

(add-hook 'Info-mode-hook #'mixed-pitch-mode)

(use-package! org
  :commands org-mode
  :config
  (map! :leader
        (:prefix ("f" . "file")
         :desc "Open init.org" "o" '(lambda () (interactive) (find-file "~/org/org.org"))))
  (setq org-bullets-bullet-list '(" "))
  (setq org-cycle-separator-lines 1)
  (setq org-edit-src-content-indentation 0)
  (setq org-ellipsis "  ")
  (setq org-export-initial-scope 'subtree)
  (setq org-image-actual-width 400)
  (setq org-src-window-setup 'current-window)
  (setq org-startup-indented t)
  (setq org-bullets-bullet-list '("·")))

(use-package! org-bullets
  :commands org-bullets-mode
  :after org
  :config
  (setq org-bullets-bullet-list
        '("⁖"))
  :hook (org-mode . org-bullets-mode))

;; If changing such values, you must reload the theme
(use-package! modus-operandi-theme
  :defer t
  :init
  (setq modus-operandi-theme-scale-headings t)
  (setq modus-operandi-theme-diffs 'desaturated)
  (setq modus-operandi-theme-intense-paren-match t))

(after! deft
  (setq deft-directory "~/org/roam"))

(after! doom-modeline
 (setq doom-modeline-persp-name t
       doom-modeline-persp-icon t))

;;;; Outline

(use-package bicycle
    :after outline)

(map! :map outline-minor-mode-map
    :n "<tab>" #'bicycle-cycle
    :n "<backtab>" #'bicycle-cycle-global)

(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook 'outline-minor-mode)
  (add-hook 'prog-mode-hook 'hs-minor-mode))


(use-package outline-minor-faces
  :hook
  (outline-minor-mode-hook . outline-minor-faces-add-font-lock-keywords))
