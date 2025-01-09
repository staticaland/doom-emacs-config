;;; config.el -*- lexical-binding: t; -*-

(setq user-full-name "Anders K. Pettersen"
      user-mail-address "staticaland@users.noreply.github.com")

(setq! doom-font (font-spec :family "MesloLGM Nerd Font Mono" :size 15)
       doom-variable-pitch-font (font-spec :family "EB Garamond" :size 22)
       doom-big-font (font-spec :family "MesloLGM Nerd Font Mono" :size 18))

(use-package! mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode)
  (markdown-mode . mixed-pitch-mode)
  :config
  (setq! mixed-pitch-set-height t))

(after! projectile
  (setq +workspaces-on-switch-project-behavior t)

  (setq projectile-ignored-projects '("~/" "/tmp" "~/.emacs.d/.local/straight/repos/"))

  (defun projectile-ignored-project-function (filepath)
    "Return t if FILEPATH is within any of `projectile-ignored-projects'"
    (cl-some (lambda (p) (s-starts-with-p p filepath)) projectile-ignored-projects)))

(after! dired
  (setq ls-lisp-dirs-first t)

  (setq dired-listing-switches
        (if (executable-find "gls")
            "-lFaGh1vt --group-directories-first"  ; gls version with all features
            "-lathlF"))                            ; standard ls version with all features

  (add-hook! 'dired-mode-hook 'dired-hide-details-mode)
  (add-hook! 'dired-mode-hook 'hl-line-mode)

  (put 'dired-find-alternate-file 'disabled nil)
  (setq delete-by-moving-to-trash t)
  (setq dired-dwim-target t)
  (setq dired-recursive-copies (quote always))
  (setq dired-recursive-deletes (quote top)))

(add-hook! cue-mode
  (cuefmt-on-save-mode))

(add-hook! go-mode
  (gofmt-on-save-mode))

(add-hook! terraform-mode
  (terraform-format-on-save-mode))

(after! org
  (setq org-imenu-depth 7)
  (setq org-ellipsis " ▾ ")
  (setq org-superstar-headline-bullets-list '("⁖"))
  (setq org-babel-python-command "python3")
  (setq org-cycle-separator-lines 1)
  (setq org-edit-src-content-indentation 0)
  (setq org-export-initial-scope 'subtree)
  (setq org-image-actual-width 400)
  (setq org-src-window-setup 'current-window)
  (setq org-startup-indented t))

(use-package! ox-gfm
  :after org)

(use-package! treemacs
  :commands treemacs
  :init
    (map! :leader
      (:prefix ("f" . "file")
        :desc "Open Treemacs" "t" #'+treemacs/toggle))
  :config
  (treemacs-git-mode 'extended)
  (setq treemacs-is-never-other-window nil)
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?))

(use-package! outline
  :config
  (setq outline-blank-line t))

(use-package! replace
  :init
    (map! :map occur-mode-map
      :n  "e" 'occur-edit-mode)

  (add-hook 'occur-hook
          '(lambda ()
             (switch-to-buffer-other-window "*Occur*"))))

(use-package! web-mode
  :mode (("\\.html$" . web-mode)
         ("\\.tmpl\\'" . web-mode)))

(add-hook! web-mode
  (prettier-on-save-mode))

(use-package! auth-source
  :no-require t
  :config (setq! auth-sources '("~/.authinfo")))

(use-package! gptel
  :defer t
  :init
  ;; Pre-configured chat configurations
  (defvar gptel-chat-configs
    '(("terraform" . ((name . "*GPTel-Terraform*")
                      (model . gpt-4o)
                      (system-message . "You are an expert Terraform developer focusing on AWS infrastructure. Provide idiomatic Terraform code with clear explanations. Include best practices for infrastructure as code, AWS-specific optimizations, and security considerations. Use modern Terraform syntax and AWS provider features.")
                      (temperature . 0.7)))
      ("golang" . ((name . "*GPTel-Golang*")
                   (model . gpt-4o)
                   (system-message . "You are an expert Go developer. Provide idiomatic Go code that follows the language's best practices and conventions. Focus on writing clean, efficient, and well-documented code. Include proper error handling, concurrent programming patterns when relevant, and follow the official Go style guide.")
                   (temperature . 0.7)))
      ("python" . ((name . "*GPTel-Python*")
                   (model . gpt-4o)
                   (system-message . "You are an expert Python developer. Write Pythonic code following PEP-8 guidelines. Focus on clean, readable, and maintainable solutions. Use modern Python features and best practices. Include type hints, docstrings, and appropriate error handling.")
                   (temperature . 0.7)))))

  (defun gptel-create-chat (config-key)
    "Create or switch to a pre-configured gptel chat buffer.
CONFIG-KEY is a string matching a key in `gptel-chat-configs'."
    (interactive
     (list (completing-read "Select chat configuration: "
                           (mapcar #'car gptel-chat-configs))))
    (if-let* ((config (alist-get config-key gptel-chat-configs nil nil #'equal))
              (buffer-name (alist-get 'name config))
              (model (alist-get 'model config))
              (system-message (alist-get 'system-message config))
              (temperature (alist-get 'temperature config)))
        (progn
          (with-current-buffer (gptel buffer-name)
            (setq-local gptel-model model
                        gptel--system-message system-message
                        gptel-temperature temperature)
            (current-buffer))
          ;; Switch to the buffer using Doom's display function
          (pop-to-buffer buffer-name)
          (message "Created %s chat buffer with %s model" config-key model))
      (user-error "No configuration found for %s" config-key)))

  (map! :leader
        (:prefix ("j" . "ai")
         :desc "Open chat" "c" #'gptel
         :desc "Add file/buffer to context" "a" #'gptel-add
         :desc "Open menu" "m" #'gptel-menu
         :desc "Submit prompt" "j" #'gptel-send
         :desc "Submit prompt with prefix arg" "s" (cmd! (gptel-send t))
         (:prefix ("o" . "open specialized chat")
          :desc "Terraform Chat" "t" (cmd! (gptel-create-chat "terraform"))
          :desc "Golang Chat" "g" (cmd! (gptel-create-chat "golang"))
          :desc "Python Chat" "p" (cmd! (gptel-create-chat "python")))))

  :config
  (setq! gptel-default-mode 'org-mode)
  (setq! gptel-prompt-prefix-alist
      '((markdown-mode . "# Prompt\n\n")
        (org-mode . "* Prompt\n\n")))

  (setq! gptel-response-prefix-alist
      '((markdown-mode . "# Response**\n\n")
        (org-mode . "* Response\n\n")))

  (add-hook! 'gptel-mode-hook
    (when (eq major-mode 'org-mode)
      (+org-pretty-mode 1)))

  (add-hook! 'gptel-post-response-functions 'gptel-end-of-response)

  (map! :map gptel-mode-map
        "C-c C-c" #'gptel-send))

(use-package! aider
  :after doom auth-source
  :config
  ;; Securely retrieve API keys from auth-source
  (let ((openai-key (auth-source-pick-first-password :host "api.openai.com" :user "apikey"))
        (anthropic-key (auth-source-pick-first-password :host "api.anthropic.com" :user "apikey")))
    ;; Set environment variables for API access
    (when openai-key
      (setenv "OPENAI_API_KEY" openai-key))
    (when anthropic-key
      (setenv "ANTHROPIC_API_KEY" anthropic-key)))
  
  ;; Use the Sonnet model for improved code generation
  (setq! aider-args '("--sonnet"))

  ;; Disable company-mode in aider buffers to prevent interference
  (add-hook! 'comint-mode-hook
    (defun +aider-disable-company-h ()
      (when (string-match-p "\\*aider:" (buffer-name))
        (company-mode -1)))))

(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . 'copilot-accept-completion)
              ("TAB" . 'copilot-accept-completion)
              ("C-TAB" . 'copilot-accept-completion-by-word)
              ("C-<tab>" . 'copilot-accept-completion-by-word)))

(after! makefile-mode
  (setq-hook! 'makefile-mode-hook indent-tabs-mode t))

;;;###autoload
(defun my/open-in-ghostty (&optional path)
  "Open PATH in Ghostty. If PATH is nil, open the current directory."
  (interactive)
  (let* ((path (expand-file-name
                (or path default-directory)))
         (quoted-path (shell-quote-argument path))
         (command (format "open -a Ghostty %s" quoted-path)))
    (message "Running: %s" command)
    (shell-command command)))

;;;###autoload
(defun my/open-current-dir-in-ghostty ()
  "Open current directory in Ghostty."
  (interactive)
  (my/open-in-ghostty default-directory))

;;;###autoload
(defun my/open-project-in-ghostty ()
  "Open project root directory in Ghostty."
  (interactive)
  (my/open-in-ghostty (doom-project-root default-directory)))

(map! :leader
      (:prefix ("o" . "open")
       :desc "Open dir in Ghostty" "i" #'my/open-current-dir-in-ghostty
       :desc "Open project in Ghostty" "I" #'my/open-project-in-ghostty))

(after! transient
  (transient-define-prefix merge-conflict-menu ()
    "Menu for resolving merge conflicts"
    [["Movement"
      ("n" "Next" smerge-next)
      ("p" "Previous" smerge-prev)]
     ["Keep"
      ("u" "Your changes (on current branch) (upper)" smerge-keep-mine)
      ("l" "Their changes (from incoming branch) (lower)" smerge-keep-other)
      ("b" "Common ancestor (last shared commit) (base)" smerge-keep-base)
      ("a" "All versions combined" smerge-keep-all)
      ("RET" "Version at cursor position" smerge-keep-current)]
     ["Compare"
      ("=" "Your changes vs their changes" smerge-diff-upper-lower)
      ("<" "Common ancestor vs your changes" smerge-diff-base-upper)
      (">" "Common ancestor vs their changes" smerge-diff-base-lower)]
     ["Resolve"
      ("r" "Auto-resolve if possible" smerge-resolve)
      ("k" "Remove conflict markers" smerge-kill-current)]]))

(map! :leader
      (:prefix ("g" . "git")
       :desc "Merge conflict menu" "d" #'merge-conflict-menu))
