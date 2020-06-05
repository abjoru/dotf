let g:airline_theme='deus'
let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#formatter = 'unique_tail'

function! Tabbufn()
    let s:tab_count = tabpagenr('$')
    if s:tab_count <= 1
        :bn
    else
        :tabnext
    endif
endfunction

function! Tabbufp()
    let s:tab_count = tabpagenr('$')
    if s:tab_count <= 1
        :bp
    else
        :tabprev
    endif
endfunction

" Use <leader>[ and <leader>] to switch tabs
nnoremap <silent> <LocalLeader>[ :call Tabbufp()<CR>
nnoremap <silent> <LocalLeader>] :call Tabbufn()<CR>
