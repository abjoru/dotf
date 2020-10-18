" Helper API for setting up key bindings

"""""""
" API "
"""""""

command! -nargs=+ -bar DfBind call DotfBufBind(<args>)
command! -nargs=+ -bar DfMap call DotfBufMap(<args>)
command! -nargs=+ -bar DfNMap call DotfBufNMap(<args>)
command! -nargs=+ -bar DfVMap call DotfBufVMap(<args>)

command! -nargs=+ -bar DfFileTypeBind call DotfFileTypeBind(<args>)
command! -nargs=+ -bar DfFileTypeMap call DotfFileTypeMap(<args>)
command! -nargs=+ -bar DfFileTypeNMap call DotfFileTypeNMap(<args>)

function! s:debug(msg)
  if g:dotf_module_debug
    echom a:msg
  endif
endfunction

function! DotfBufBind(map, binding, name, value, isCmd)
  call s:debug('+++ Adding BufEnter mapping to key ' . a:binding . ' for cmd: ' . a:name)

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

function! DotfBufMap(binding, name, value, ...)
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif
  call DotfBufBind('map', a:binding, a:name, a:value, l:isCmd)
endfunction

function! DotfBufNMap(binding, name, value, ...)
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif
  call DotfBufBind('nmap', a:binding, a:name, a:value, l:isCmd)
endfunction 

function! DotfBufVMap(binding, name, value, ...)
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif
  call DotfBufBind('vmap', a:binding, a:name, a:value, l:isCmd)
endfunction 

function! DotfFileTypeBind(ft, map, binding, name, value, isCmd)
  call s:debug('-+- Adding FT mapping to key ' . a:binding . ' for cmd: ' . a:name . ' for file type ' . a:ft)

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

function! DotfFileTypeMap(tfile, binding, name, value, ...)
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif

  call DotfFileTypeBind(a:tfile, 'map', a:binding, a:name, a:value, l:isCmd)
endfunction

function! DotfFileTypeNMap(tfile, binding, name, value, ...)
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif

  call DotfFileTypeBind(a:tfile, 'nmap', a:binding, a:name, a:value, l:isCmd)
endfunction
