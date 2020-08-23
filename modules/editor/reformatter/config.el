;;; editor/reformatter/config.el -*- lexical-binding: t; -*-

(use-package! reformatter
  :config
  (reformatter-define gofmt :program "gofmt" :args '(""))
  (reformatter-define whaturl-org :program "whaturl" :args '("--format" "org"))
  (reformatter-define whaturl-md :program "whaturl" :args '("--format" "md"))
  (reformatter-define prettier-css :program "prettier" :args '("--parser=css"))
  (reformatter-define prettier-html :program "prettier" :args '("--parser=html"))
  (reformatter-define prettier-javascript :program "prettier" :args '("--parser=babylon"))
  (reformatter-define prettier-json :program "prettier" :args '("--parser=json"))
  (reformatter-define prettier-markdown :program "prettier" :args '("--parser=markdown"))
  (reformatter-define prettier-typescript :program "prettier" :args '("--parser=typescript"))
  (reformatter-define prettier-yaml :program "prettier" :args '("--parser=yaml"))
  (reformatter-define refang :program "defang" :args '("--refang"))
  (reformatter-define defang :program "defang" :args '(""))
  (reformatter-define scalafmt :program "scalafmt" :args '("--stdin"))
  (reformatter-define terraform :program "terraform" :args '("fmt" "-"))
  (reformatter-define black
    :program "black"
    :args '("-" "--quiet"))
  (reformatter-define isort-format
    :program "isort"
    :args '("--apply" "-"))
  (reformatter-define json-format
    :program "jq"
    :args '("--indent" "4"))
  (reformatter-define marklink-format
    :program "marklink"
    :args '("--format" "org"))
  (reformatter-define xml-format
    :program "xmllint"
    :args '("--format" "-")
    :mode nil))
