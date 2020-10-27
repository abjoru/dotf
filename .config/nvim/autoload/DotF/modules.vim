let s:MODULES = DotF#api#import('modules')

" Return the shared modules instance
function! DotF#modules#instance() abort
  return s:MODULES
endfunction

" Tests if a named module has been enabled
function! DotF#modules#is_enabled(name) abort
  if index(s:MODULES.list_enabled_modules(), a:name) != 1
    return 1
  endif
endfunction
