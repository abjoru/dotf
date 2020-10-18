# Search Module

This module provides fuzzy search functionality with FZF:

- [`junegunn/fzf`](https://github.com/junegunn/fzf)
- [`junegunn/fzf.vim`](https://github.com/junegunn/fzf.vim)

## Install
Add the `nav/search` module to your `init.vim` file.

```viml
function! Modules()
  " ...
  Module 'nav/search'
  " ...
endfunction
```

## Key Bindings
Key Binding | Description
SPC f b     | Find open buffer
SPC f f     | Find file
SPC f g     | Find line using ripgrep (if available)
SPC f G     | Find line using git grep (if `scm/git` is enabled)
SPC f l     | Find line in loaded buffers
