# Base Configuration Module

This module provides the base configuration (and plugins), and provides a sensible experience. It enables the following packages:

- [`vviikk/vim-leader-guide-redux`](https://github.com/vviikk/vim-leader-guide-redux)

## Install

Add the `core/base` module in your `init.vim` file.

```viml
function! Modules()
  " ...
  Module 'core/base'
  " ...
endfunction
```

## Key Bindings
Key Binding | Mode   | Description
Ctrl j      | normal | Navigate window down
Ctrl k      | normal | Navigate window up
Ctrl l      | normal | Navigate window left
Ctrl h      | normal | Navigate window right
<           | visual | Move code block
>           | visual | Move code block
SPC SPC     | normal | Clear search highlighting
