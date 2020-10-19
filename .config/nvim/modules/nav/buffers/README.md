# Buffers Module
This module provides common buffer functionality using:

- [`schickling/vim-bufonly`](https://github.com/schickling/vim-bufonly)

## Install
Add the `nav/buffers` module in your `init.vim` configuration file.

```viml
function! Modules()
  " ...
  Module 'nav/buffers'
  " ...
endfunction
```

## Key Bindings
Key Binding | Description
SPC b d     | Kill the current buffer
SPC b D     | Force kill the current buffer
SPC b o     | Kill all buffers except active one
SPC b n     | Go to next buffer
SPC b p     | Go to previous buffer
SPC b N     | Open new empty buffer
SPC b V     | Open new empty buffer in a vertial split
SPC b l     | List all buffers
SPC b F     | Open first buffer
SPC b L     | Open last buffer
SPC b h     | Open home screen
SPC b C     | Copy whole contents of the buffer
