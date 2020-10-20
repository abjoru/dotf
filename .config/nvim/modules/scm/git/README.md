# Git Module
This module provides common functionality for git, using
- [`airblade/vim-gitgutter`](https://github.com/airblade/vim-gitgutter)
- [`junegunn/gv.vim`](https://github.com/junegunn/gv.vim)
- [`tpope/vim-fugitive`](https://github.com/tpope/vim-fugitive)

## Install
Add the `scm/git` module to the `init.vim` configuration file.

```viml
function! Modules()
  " ...
  Module 'scm/git'
  " ...
endfunction
```

## Key Bindings
Key Binding | Description
SPC g c     | Commit a change
SPC g l     | Log latest changes
SPC g L     | Log current file
SPC g p     | Push current branch
SPC g s     | Status
