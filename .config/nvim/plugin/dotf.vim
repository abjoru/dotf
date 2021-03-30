"""""""""""""""""""""""""""""""
" Nice-to-have debug mappings "
"""""""""""""""""""""""""""""""

" Show highlighting group for word under cursor (syntax debugging)
map <F5> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" Show syntax stack under cursor
nmap <F6> :call <SID>SynStack()<CR>

function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc
