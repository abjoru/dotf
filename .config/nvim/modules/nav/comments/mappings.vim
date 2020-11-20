let g:lmap.c = get(g:lmap, 'c', {'name': 'Comments'})

nnoremap <leader>cl :gcc<CR>
vnoremap <leader>cl :gc<CR>
let g:lmap.c.l = 'Comment/uncomment lines'

nnoremap <leader>cy :yygcc<CR>
vnoremap <leader>cy :ygvgc<CR>
let g:lmap.c.y = 'Copy and comment lines'

noremap <leader>cc :DfInvertToggleComment<CR>
let g:lmap.c.c = 'Inverted comment/uncomment lines'

" Comment boxes by filetype
" FIXME broken for some reason. See core/base/mappings.vim
"autocmd FileType vim vnoremap <buffer> <leader>cb !boxes -f ~/.config/nvim/cfg/boxes -d vim-box<CR>
"autocmd FileType scala vnoremap <buffer> <leader>cb !boxes -f ~/.config/nvim/cfg/boxes -d scala<CR>
"let g:lmap.c.b = 'Box comment'
