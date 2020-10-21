
""""""""""""""""""""""""""""
" DotF Theme Configuration "
""""""""""""""""""""""""""""

""
" Sets the vim theme (and optionally airline theme if enabled)
function! DotF#themes#set(bg, name, ...) abort
  " Set background
  if a:bg ==? 'light'
    set background=light
  else
    set background=dark
  endif

  try
    " Set vim theme
    if has('termguicolors')
      set termguicolors
    endif

    execute 'colorscheme ' . a:name
    hi Comment cterm=italic

    " Set airline theme if available
    if a:0 ==? 1
      let g:airline_theme = a:1
    endif

    " attempt to refresh airline
    if exists(':AirlineRefresh')
      :AirlineRefresh
    endif
  catch
  endtry
endfunction
