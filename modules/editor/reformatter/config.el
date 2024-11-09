;;; editor/reformatter/config.el -*- lexical-binding: t; -*-

(use-package! reformatter
  :defer 5
  :config
  (reformatter-define cuefmt :program "cue" :args '("fmt" "-" "-s"))
  (reformatter-define whaturl-org :program "whaturl" :args '("--format" "org"))
  (reformatter-define whaturl-md :program "whaturl" :args '("--format" "markdown"))
  (reformatter-define refang :program "defang" :args '("--refang"))
  (reformatter-define defang :program "defang" :args '(""))
  (reformatter-define scalafmt :program "scalafmt" :args '("--stdin"))
  (reformatter-define terraform-format :program "terraform" :args '("fmt" "-"))
  (reformatter-define gofmt :program "gofmt")
  (reformatter-define black :program "black" :args '("-" "--quiet"))
  (reformatter-define isort-format :program "isort" :args '("--apply" "-"))
  (reformatter-define prettier
    :program "prettier"
    :args (list "--stdin-filepath" (buffer-file-name)))
  (reformatter-define json-format :program "jq" :args '("--indent" "4"))
  (reformatter-define xml-format :program "xmllint" :args '("--format" "-") :mode nil))
