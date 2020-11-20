let g:lmap.b = get(g:lmap, 'b', {'name': 'Buffers'})

nnoremap <leader><C-I> :b#<CR>
let g:lmap['<C-I>'] = "last-buffer"

"DfNMap 'bd', 'Kill this buffer', 'bd'
nnoremap <leader>bd :bd<CR>
let g:lmap.b.d = 'Kill this buffer'

"DfNMap 'bD', 'Force kill this buffer', 'bd!'
nnoremap <leader>bD :bd!<CR>
let g:lmap.b.D = 'Force kill this buffer'

"DfNMap 'bo', 'Kill other buffers', 'BufOnly'
nnoremap <leader>bo :BufOnly<CR>
let g:lmap.b.o = 'Kill other buffers'

" Switch to next buffer
nnoremap <leader>bn :bnext!<CR>
let g:lmap.b.n = 'Next buffer'

"DfNMap 'bp', 'Previous buffer', 'bprevious!'
nnoremap <leader>bp :bprevious!<CR>
let g:lmap.b.p = 'Previous buffer'

"DfNMap 'bN', 'New empty buffer', 'new'
nnoremap <leader>bN :new<CR>
let g:lmap.b.N = 'New empty buffer'

"DfNMap 'bV', 'New empty vertical buffer', 'vnew'
nnoremap <leader>bV :vnew<CR>
let g:lmap.b.V = 'New empty vertical buffer'

"DfNMap 'bl', 'List all buffers', 'buffers'
nnoremap <leader>bl :buffers<CR>
let g:lmap.b.l = 'List all buffers'

"DfNMap 'bF', 'Open first buffer', 'bfirst!'
nnoremap <leader>bF :bfirst!<CR>
let g:lmap.b.F = 'Open first buffer'

"DfNMap 'bL', 'Open last buffer', 'blast!'
nnoremap <leader>bL :blast!<CR>
let g:lmap.b.L = 'Open last buffer'

"DfNMap 'bC', 'Copy whole buffer', '%y *'
nnoremap <leader>bC :%y *<CR>
let g:lmap.b.C = 'Copy whole buffer'

"DfNMap 'bh', 'Startpage', 'Startify'
nnoremap <leader>bh :Startify<CR>
let g:lmap.b.h = 'Home'
