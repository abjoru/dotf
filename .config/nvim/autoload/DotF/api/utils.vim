let s:self = {}

function! DotF#api#utils#get() abort
  return deepcopy(s:self)
endfunction

function! s:self.source(file) abort
  execute 'source ' . a:file
endfunction

" Tests if python is enabled
" return: true if enabled, false otherwise
function! s:self.has_python() abort
  return has('python') || has('python3')
endfunction
