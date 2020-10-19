function! TabBufN()
  let s:tab_count = tabpagenr('$')
  if s:tab_count <= 1
    :bn
  else
    :tabnext
  endif
endfunction

function! TabBufP()
  let s:tab_count = tabpagenr('$')
  if s:tab_count <= 1
    :bp
  else
    :tabprev
  endif
endfunction
