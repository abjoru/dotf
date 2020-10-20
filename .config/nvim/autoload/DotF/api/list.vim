function! DotF#api#list#get() abort
  return map({
    \ 'sort': '',
    \ 'reverse': '',
    \ 'append': '',
    \ 'replace': '',
    \ 'pop': '',
    \ 'map': '',
    \ 'filter': '',
    \ 'filterNot': ''
    \ }, "function('s:' . v:key)")
endfunction

function! s:sort(list) abort
  let nls = deepcopy(a:list)
  call sort(nls)
  return nls
endfunction

function! s:reverse(list) abort
  let nls = deepcopy(a:list)
  call reverse(nls)
  return nls
endfunction

function! s:append(list, val) abort
  let nls = deepcopy(a:list)
  call add(nls, a:val)
  return nls
endfunction

function! s:replace(list, index, val) abort
  let nls = deepcopy(a:list)
  let nls[a:index] = a:val
  return nls
endfunction

function! s:pop(list, index) abort
  let nls = deepcopy(a:list)
  call remove(nls, a:index)
  return nls
endfunction

function! s:map(fn, list) abort
  let nls = deepcopy(a:list)
  call map(nls, string(a:fn) . '(v:val)')
  return nls
endfunction

function! s:filter(fn, list) abort
  let nls = deepcopy(a:list)
  call filter(nls, string(a:fn) . '(v:val)')
  return nls
endfunction

function! s:filterNot(fn, list) abort
  let nls = deepcopy(a:list)
  call filter(nls, '!' . string(a:fn) . '(v:val)')
  return nls
endfunction
