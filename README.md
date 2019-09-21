# Tridash Mode

This is an emacs major mode for editing Tridash source files,
supporting rudimentary syntax highlighting and automatic indentation.

_This mode is still in very early stages of development and may
contain bugs._

## Usage

Copy the `tridash-mode.el` file somewhere in your `load-path` and add
the following to your `.emacs`:

```lisp
(require 'tridash-mode)

(add-to-list 'auto-mode-alist '("\\.trd\\'" . tridash-mode))
```
