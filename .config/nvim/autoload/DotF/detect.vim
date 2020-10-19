let s:LOG = DotF#logger#derive('detect')

let s:cache_dir = expand(resolve($HOME . '/.cache/nvim'))
let s:cache_file = expand(resolve(s:cache_dir . '/loaded-plugins.vim'))

function! DotF#detect#sync() abort
  call s:detect_plugin_changes()
  call DotF#updates#sync()
endfunction

function! s:detect_plugin_changes() abort
  if !isdirectory(s:cache_dir)
    call mkdir(s:cache_dir, 'p')
  endif

  if filereadable(s:cache_file)
    let l:old_plugins = readfile(s:cache_file)
  else
    let l:old_plugins = []
  endif

  if l:old_plugins == DotF#modules#enabledplugins()
    call s:LOG.info('No changes in plugins')
  else
    call s:LOG.info('Plugins changes detected, installing...')
    call DotF#modules#install()
  endif
endfunction
