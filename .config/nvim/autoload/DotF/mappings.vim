"""""""""""""""""""""
" Mapping Functions "
"""""""""""""""""""""

function! DotF#mappings#buf_bind(map, binding, name, value, isCmd) abort
  if a:isCmd
    let l:value = ':' . a:value . '<cr>'
  else
    let l:value = a:value
  endif

  if a:map ==# 'map' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'noremap'
  elseif a:map ==# 'nmap' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'nnoremap'
  elseif a:map ==# 'vmap' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'vnoremap'
  elseif a:map ==# 'tmap' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'tnoremap'
  else
    let l:noremap = ''
  endif

  if l:noremap !=# ''
    execute 'au VimEnter * ' . l:noremap . ' <SID>' . a:name . '# ' . l:value
    execute 'au VimEnter * ' . a:map . ' <Leader>' . a:binding . ' <SID>' . a:name . '#'
  endif
endfunction

function! DotF#mappings#buf_map(binding, name, value, ...) abort
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif

  call DotF#mappings#buf_bind('map', a:binding, a:name, a:value, l:isCmd)
endfunction

function! DotF#mappings#buf_nmap(binding, name, value, ...) abort
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif

  call DotF#mappings#buf_bind('nmap', a:binding, a:name, a:value, l:isCmd)
endfunction

function! DotF#mappings#buf_vmap(binding, name, value, ...) abort
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif

  call DotF#mappings#buf_bind('vmap', a:binding, a:name, a:value, l:isCmd)
endfunction

""""""""""""""""""""""
" Filetype Functions "
""""""""""""""""""""""

function! DotF#mappings#ft_bind(ft, map, binding, name, value, isCmd) abort
  if a:isCmd
    let l:value = ':' . a:value . '<cr>'
  else
    let l:value = a:value
  endif

  if a:map ==# 'map' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'noremap'
  elseif a:map ==# 'nmap' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'nnoremap'
  elseif a:map ==# 'vmap' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'vnoremap'
  elseif a:map ==# 'tmap' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'tnoremap'
  else
    let l:noremap = ''
  endif

  if l:noremap !=# ''
    execute 'au VimEnter * if &ft ==? "' . a:ft . '" | ' . l:noremap . ' <buffer> <SID>' . a:name . '# ' . l:value . ' | endif '
    execute 'au VimEnter * if &ft ==? "' . a:ft . '" | ' . a:map . ' <buffer> <Leader>' . a:binding . ' <SID>' . a:name . '# | endif '

    execute 'au FileType ' . a:ft . ' ' . l:noremap . ' <buffer> <SID>' . a:name . '# ' . l:value
    execute 'au FileType ' . a:ft . ' ' . a:map . ' <buffer> <Leader>' . a:binding . ' <SID>' . a:name . '#'
  endif
endfunction

function! DotF#mappings#ft_map(tfile, binding, name, value, ...) abort
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif

  call DotF#mappings#ft_bind(a:tfile, 'map', a:binding, a:name, a:value, l:isCmd)
endfunction

function! DotF#mappings#ft_nmap(tfile, binding, name, value, ...) abort
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif

  call DotF#mappings#ft_bind(a:tfile, 'nmap', a:binding, a:name, a:value, l:isCmd)
endfunction
