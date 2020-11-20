DfLMap 'b', 'Buffers'

nnoremap <leader><C-I> :b#<CR>

let g:lmap['<C-I>'] = "last-buffer"

DfNMap 'bd', 'Kill this buffer', 'bd'
DfNMap 'bD', 'Force kill this buffer', 'bd!'
DfNMap 'bo', 'Kill other buffers', 'BufOnly'
DfNMap 'bn', 'Next buffer', 'bnext!'
DfNMap 'bp', 'Previous buffer', 'bprevious!'
DfNMap 'bN', 'New empty buffer', 'new'
DfNMap 'bV', 'New empty vertical buffer', 'vnew'
DfNMap 'bl', 'List all buffers', 'buffers'
DfNMap 'bF', 'Open first buffer', 'bfirst!'
DfNMap 'bL', 'Open last buffer', 'blast!'
DfNMap 'bC', 'Copy whole buffer', '%y *'
DfNMap 'bh', 'Startpage', 'Startify'
