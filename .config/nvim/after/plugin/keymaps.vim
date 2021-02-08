call which_key#register('<Space>', 'g:lmap')

let g:lmap = {}
let g:lmap.a = 'Open Diagnostics'
let g:lmap.d = 'Go to Diagnostics'

" Buffers
let g:lmap.b = {
      \ 'name': '+Buffers',
      \ 'd': 'Delete buffer',
      \ 'o': 'Close other buffers',
      \ 's': 'Select tab'
      \ }

" Search
let g:lmap.f = {
      \ 'name': '+Files (FZF)',
      \ 'f': 'Files',
      \ 'b': 'Buffers',
      \ 'g': 'Words',
      \ 'r': 'Tree reveal'
      \}

" Comments
let g:lmap.c = {
      \ 'name': '+Comments'
      \}

" Git
let g:lmap.g = {
      \ 'name': '+Git',
      \ 'c': 'Commit',
      \ 'd': 'Diff',
      \ 'l': 'Logs',
      \ 's': 'Status',
      \ 'L': 'Commit logs'
      \}
