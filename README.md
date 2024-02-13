# Tridash Mode

> [!IMPORTANT]  
> Tridash will no longer be developed. Future development will focus on the [Live Cells](https://docs.page/alex-gutev/live_cells) project, which incorporates some of the ideas presented here.

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
