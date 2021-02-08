" Get icons from nvim-web-devicons plugin
function! DotF#GetIcon(...)
  if a:0 == 0
    let ext = expand('%:e')
    let nam = expand('%:t')
  else
    let ext = fnamemodify(a:1, ':e')
    let nam = fnamemodify(a:1, ':t')
  endif

  return luaeval('require("nvim-web-devicons").get_icon("' . nam . '", "' . ext . '", {default = true})')
endfunction

"""""""""""""""""""""""""""""""
" Nice-to-have debug mappings "
"""""""""""""""""""""""""""""""

" Show highlighting group for word under cursor (syntax debugging)
map <F5> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
