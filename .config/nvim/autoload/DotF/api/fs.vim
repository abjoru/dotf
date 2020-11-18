function! DotF#api#fs#get() abort
  return map({
    \ 'find': ''
    \ }, "function('s:' . v:key)")
endfunction

" Find files matching glob in dir
" Return: list of matches or empty
function! s:find(dir, glob) abort
  return split(globpath(a:dir, a:glob), '\n')
endfunction
