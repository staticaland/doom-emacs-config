# Doom Emacs Configuration

This is a Doom Emacs configuration.

Main config is `config.org`. It includes other Org files.

## Debugging workflow

Use `emacsclient` commands to inspect live state.

You can also add code blocks (with `tangle: no`). Then use `org-babel-execute-src-block` as described below.

## Executing org-babel code blocks

Execute specific code blocks by line number:

```sh
emacsclient --eval "(save-excursion (find-file \"path/to/file.org\") (goto-line LINE_NUMBER) (org-babel-execute-src-block))"
```

## Common debug commands:

```sh
emacsclient --eval "(doom-initialized-p)"
emacsclient --eval "doom-modules"
emacsclient --eval "(with-current-buffer \"_Messages_\" (buffer-string))"
emacsclient --eval "(key-binding (kbd \"SPC f f\"))"
```
