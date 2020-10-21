" API
let g:airline_theme='gruvbox'
let g:airline#extensions#tabline#enabled = g:dotf_ui_statusbar_tabline_enabled
let g:airline#extensions#tabline#show_tabs = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'

nnoremap <silent> <LocalLeader>[ :call TabBufP()<CR>
nnoremap <silent> <LocalLeader>] :call TabBufN()<CR>
