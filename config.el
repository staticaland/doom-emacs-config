;;; config.el -*- lexical-binding: t; -*-

(setq user-full-name "Anders K. Pettersen"
      user-mail-address "john@doe.com")

(setq display-line-numbers-type nil)

(setq doom-theme 'modus-operandi)

(setq doom-themes-treemacs-enable-variable-pitch nil)

(when (eq system-type 'darwin)
  (setq doom-font (font-spec :family "Menlo" :size 14))
  (setq doom-big-font (font-spec :family "Menlo" :size 36))
  (setq doom-big-font-increment 4)
  (setq doom-serif-font (font-spec :family "EtBembo"))
  (setq doom-variable-pitch-font (font-spec :family "EtBembo"))
  (font-put doom-font :weight 'semi-light))

(when (eq system-type 'gnu/linux)
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append))

(use-package evil
  :init
  (setq evil-want-Y-yank-to-eol t))

(after! evil-snipe
  (setq evil-snipe-scope 'visible))

(after! projectile
  (setq +workspaces-on-switch-project-behavior t)

  (setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))
  (defun projectile-ignored-project-function (filepath)
      "Return t if FILEPATH is within any of `projectile-ignored-projects'"
      (or (mapcar (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects))))

(after! dired

  (add-hook! 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook! 'dired-mode-hook 'hl-line-mode)

  (if (executable-find "gls")
      (progn
        (setq insert-directory-program "gls")
        (setq dired-listing-switches "-lFaGh1v --group-directories-first"))
    (setq dired-listing-switches "-ahlF"))

  (setq ls-lisp-dirs-first t)

  (setq dired-listing-switches "-lat") ; sort by date (new first)
  (put 'dired-find-alternate-file 'disabled nil)

  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies (quote always))
  (setq dired-recursive-deletes (quote top)))

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

(use-package! treemacs
  :commands treemacs
  :init
    (map! :leader
      (:prefix ("f" . "file")
        :desc "Open Treemacs" "t" #'+treemacs/toggle))
  :config
    (treemacs-git-mode 'extended)
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

(use-package! modus-operandi-theme
  :defer t
  :init
  (setq modus-operandi-theme-scale-headings t)
  (setq modus-operandi-theme-diffs 'desaturated)
  (setq modus-operandi-theme-intense-paren-match t))

(use-package focus
  :hook terraform-mode
  :config
  ;; Modes inheriting prog-mode will focus on functions.
  (add-to-list 'focus-mode-to-thing '(prog-mode . defun))
  ;; Modes inheriting text-mode will focus on sentences.
  (add-to-list 'focus-mode-to-thing '(text-mode . sentence))
  ;; Terraform
  (add-to-list 'focus-mode-to-thing '(terraform-mode . paragraph)))

(setq evil-vsplit-window-right t)
(setq evil-split-window-below t)

(defadvice! prompt-for-buffer (&rest _)
  :after '(evil-window-split evil-window-vsplit)
  (+ivy/switch-workspace-buffer))

(use-package outshine)

(use-package! outline
  :config
  (setq outline-blank-line t))

(use-package! outline-minor-faces
  :after outline
  :config (add-hook 'outline-minor-mode-hook
                    'outline-minor-faces-add-font-lock-keywords))

(use-package backline
  :after outline
  :config (advice-add 'outline-flag-region :after 'backline-update))

(use-package! pretty-outlines
  :config
  (setq pretty-outlines-ellipsis " ↴")
  ;; (setq pretty-outlines-bullets-bullet-list '("⁖"))
  :hook (outline-minor-mode . pretty-outlines-set-display-table))

(use-package! bicycle
  :config
  (map! :map outline-minor-mode-map
        :n "<tab>" #'bicycle-cycle
        :n "<backtab>" #'bicycle-cycle-global))

(add-hook 'outline-minor-mode-hook
          (defun contrib/outline-overview ()
              (outline-show-all)
              (outline-hide-body)))

(add-hook 'org-src-mode-hook
          (defun const/show-all-outlines-in-org-src ()
            (outline-show-all)))

(defun python-mode-outline-hook ()
  "Fold only definitions in Python."
  (setq-local outline-regexp
        (rx (or
             ;; Definitions
             (group (group (* space)) bow (or "class" "def" "async") eow)

             ;; Decorators
             (group (group (* space)) "@"))))
  (outline-minor-mode))

(add-hook 'python-mode-hook 'python-mode-outline-hook)

(defun terraform-mode-outline-hook ()
  (setq-local outline-regexp (rx
                        (or "resource" "data" "provider" "module" "variable" "output")
                        (one-or-more (not "{"))
                        "{"
                        line-end))
  (defun terraform-outline-level () 1)
  (setq-local outline-level 'terraform-outline-level)
  (outline-minor-mode t))


(add-hook 'terraform-mode-hook 'terraform-mode-outline-hook)

(rxt-elisp-to-pcre (rx
 (or "resource" "data" "provider" "module" "variable" "output")
 (one-or-more (not "{"))
 "{"
 line-end))

(defun c7n-outline-hook ()
  (setq-local outline-heading-alist '(("policies:" . 1)
                                ("- name:" . 2)))
  (setq-local outline-regexp (rx (or "policies:" "- name:")))
  (outline-minor-mode))

(add-hook 'yaml-mode-hook 'c7n-outline-hook)

(defun terragrunt-outline-hook ()
  (setq-local outline-regexp "^in")
  (outline-minor-mode))

(add-hook 'hcl-mode-hook 'terragrunt-outline-hook)

(use-package! olivetti
  :init
  (setq-default olivetti-body-width 0.618)
  :commands olivetti-mode)

(use-package! replace
  :init
    (map! :map occur-mode-map
      :n  "e" 'occur-edit-mode)

  (add-hook 'occur-hook
          '(lambda ()
             (switch-to-buffer-other-window "*Occur*"))))

(use-package emacs
  :config
  ;; Got those numbers from `string-to-char'
  (defconst contrib/insert-pair-alist
    '(("' Single quote" . (39 39))           ; ' '
      ("« Εισαγωγικά Gr quote" . (171 187))  ; « »
      ("\" Double quotes" . (34 34))         ; " "
      ("` Elisp quote" . (96 39))            ; ` '
      ("‘ Single apostrophe" . (8216 8217))  ; ‘ ’
      ("“ Double apostrophes" . (8220 8221)) ; “ ”
      ("( Parentheses" . (40 41))            ; ( )
      ("{ Curly brackets" . (123 125))       ; { }
      ("[ Square brackets" . (91 93))        ; [ ]
      ("< Angled brackets" . (60 62))        ; < >
      ("= Equals signs" . (61 61))           ; = =
      ("* Asterisks" . (42 42))              ; * *
      ("_ underscores" . (95 95)))           ; _ _
    "Alist of pairs for use with `prot/insert-pair-completion'.")

  (defun contrib/insert-pair-completion (&optional arg)
    "Insert pair from `contrib/insert-pair-alist'."
    (interactive "P")
    (let* ((data contrib/insert-pair-alist)
           (chars (mapcar #'car data))
           (choice (completing-read "Select character: " chars nil t))
           (left (cadr (assoc choice data)))
           (right (caddr (assoc choice data))))
      (insert-pair arg left right))))

(after! gnus
  (setq gnus-select-method '(nntp "news.gwene.org")))

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

(map! :leader
      (:prefix ("f" . "file")
       :desc "tmux cd to here" "T" #'+tmux/cd-to-here))

(use-package! elfeed
  :commands elfeed
  :init
  (map! :leader
    (:prefix ("o" . "open")
      :desc "Open elfeed" "e" #'=rss)))

(after! elfeed
  (map! :map elfeed-search-mode-map
        :localleader
        :desc "Elfeed update" "r" #'elfeed-update))

(use-package elfeed-web
    :defer t
    :commands elfeed-web-stop)

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

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

(use-package visual-regexp
  :commands vr/query-replace
  :config
  (setq vr/default-replace-preview nil)
  (setq vr/match-separator-use-custom-face t))

(use-package emoji-cheat-sheet-plus
  :commands emoji-cheat-sheet-plus-insert)

(use-package ivy-emoji
  :commands ivy-emoji)

(after! org-journal (setq org-journal-file-format "%Y%m%d.org"))

(setq org-directory "~/org/")

(map! :leader
    (:prefix ("f" . "file")
     :desc "Open init.org" "o" '(lambda () (interactive) (find-file "~/org/org.org"))))

(after! org
    (setq org-imenu-depth 7)
  (setq org-ellipsis " ▾ ")
  (setq org-superstar-headline-bullets-list '("⁖"))
  (add-hook! 'org-mode-hook #'mixed-pitch-mode)
  (add-hook! 'org-mode-hook #'olivetti-mode)
  (setq org-babel-python-command "python3")
  (setq org-cycle-separator-lines 1)
  (setq org-edit-src-content-indentation 0)
  (setq org-export-initial-scope 'subtree)
  (setq org-image-actual-width 400)
  (setq org-src-window-setup 'current-window)
  (setq org-startup-indented t))

(after! org-capture
  (setq org-capture-templates
        '(("b" "Basic task for future review" entry
           (file+headline "tasks.org" "Basic tasks that need to be reviewed")
           "* %^{Title}\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n\n%i%l"
           :empty-lines 1)

          ("w" "Work")
          ("wt" "Task or assignment" entry
           (file+headline "work.org" "Tasks and assignments")
           "\n\n* TODO [#A] %^{Title} :@work:\nSCHEDULED: %^t\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n\n%i%?"
           :empty-lines 1)

          ("wm" "Meeting, event, appointment" entry
           (file+headline "work.org" "Meetings, events, and appointments")
           "\n\n* MEET [#A] %^{Title} :@work:\nSCHEDULED: %^T\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n\n%i%?"
           :empty-lines 1)

          ("t" "Task with a due date" entry
           (file+headline "tasks.org" "Task list with a date")
           "\n\n* %^{Scope of task||TODO|STUDY|MEET} %^{Title} %^g\nSCHEDULED: %^t\n:PROPERTIES:\n:CAPTURED: %U\n:END:\n\n%i%?"
           :empty-lines 1)

          ("j" "Journal" entry
           (file+olp+datetree "journal.org")
           "* %?\n"
           :empty-lines 1)

          ("r" "Reply to an email" entry
           (file+headline "tasks.org" "Mail correspondence")
           "\n\n* TODO [#B] %:subject :mail:\nSCHEDULED: %t\n:PROPERTIES:\n:CONTEXT: %a\n:END:\n\n%i%?"
           :empty-lines 1)))

  (defun org-hugo-new-subtree-post-capture-template ()
      (let* ((title (read-from-minibuffer "Post Title: "))
           (fname (org-hugo-slug title)))
      (mapconcat #'identity
                 `(
                   ,(concat "* TODO " title)
                   ":PROPERTIES:"
                   ,(concat ":EXPORT_FILE_NAME: " fname)
                   ":END:"
                   "%?\n")
                 "\n")))

  (add-to-list 'org-capture-templates
               '("h" "Hugo blog post" entry
                 (file "~/Projects/org-blog/blog.org")
                 (function org-hugo-new-subtree-post-capture-template)
                 :empty-lines 1)))

(after! yasnippet
  (add-to-list 'yas-snippet-dirs
               (concat
                (file-name-as-directory straight-base-dir)
                (file-name-as-directory "straight")
                (file-name-as-directory "build")
                (file-name-as-directory "yasnippet-terraform/terraform-mode"))))

(add-hook! 'python-mode-hook 'poetry-tracking-mode)

(after! poetry
  (setq poetry-tracking-strategy 'projectile)
  (map! :map python-mode-map
        :localleader
        :desc "Activate Poetry tracking mode" "c" #'poetry-tracking-mode
        :desc "Restart LSP workspace" "r" #'lsp-workspace-restart
        :desc "Workon/off the Poetry venv" "w" #'poetry-venv-toggle
        :desc "Poetry menu" "p" #'poetry))
