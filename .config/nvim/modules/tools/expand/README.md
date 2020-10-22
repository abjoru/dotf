# Expand Module
This module adds expand region support to DotF using:
- [`terryma/vim-expand-region`](https://github.com/terryma/vim-expand-region)

## Install
Install the `tools/expand` module by adding it to the `init.vim` file.

```viml
function! Modules()
  " ...
  DfModule 'tools/expand'
  " ...
endfunction
```

## Key Mappings
Key Mapping | Description
+           | Expand region
_           | Shrink region
