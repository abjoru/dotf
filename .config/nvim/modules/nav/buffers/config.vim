let g:lmap.b = { 'name' : 'Buffers' }
"get(g:lmap, 'b', { 'name': 'buffers' })

nnoremap <leader><C-I> :b#<CR>

let g:lmap['<C-I>'] = "last-buffer"

DfNMap 'bd', 'kill-this-buffer', 'bd'
DfNMap 'bD', 'force-kill-this-buffer', 'bd!'
DfNMap 'bo', 'kill-other-buffers', 'BufOnly'
DfNMap 'bn', 'next-buffer', 'bnext!'
DfNMap 'bp', 'previous-buffer', 'bprevious!'
DfNMap 'bN', 'new-empty-buffer', 'new'
DfNMap 'bV', 'new-empty-vertical-buffer', 'vnew'
DfNMap 'bl', 'list-all-buffers', 'buffers'
DfNMap 'bF', 'open-first-buffer', 'bfirst!'
DfNMap 'bL', 'open-last-buffer', 'blast!'
DfNMap 'bC', 'copy-whole-buffer', '%y *'
DfNMap 'bh', 'home', 'Startify'
