let g:airline_theme='gruvbox'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'

nnoremap <silent> <LocalLeader>[ :call TabBufP()<CR>
nnoremap <silent> <LocalLeader>] :call TabBufN()<CR>
