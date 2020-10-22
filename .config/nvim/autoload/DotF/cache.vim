"""""""""""""""""""""""""
" DotF Cache Management "
"""""""""""""""""""""""""

let s:LOG = DotF#logger#derive('cache')

let s:plugin_cache_file = expand(resolve(g:cache_dir . '/loaded-plugins.vim'))

function! DotF#cache#write_plugins() abort
  if !isdirectory(g:cache_dir)
    call mkdir(g:cache_dir, 'p')
  endif

  let names = s:get_plugin_names()

  if writefile(names, s:plugin_cache_file)
    call s:LOG.error('Could not write loaded plugins to cache file!')
  else
    call s:LOG.info('Write loaded plugins: ' . string(names))
  endif
endfunction

function! DotF#cache#read_plugins() abort
  if filereadable(s:plugin_cache_file)
    let plugins = readfile(s:plugin_cache_file)
  else
    let plugins = []
  endif

  return plugins
endfunction

function! DotF#cache#has_plugins_changed() abort
  let current = sort(s:get_plugin_names())
  let cached = sort(DotF#cache#read_plugins())
  return cached != current
endfunction

function! s:get_plugin_names() abort
  let plugins = DotF#modules#list_enabled_plugins()
  return map(copy(plugins), {_, p -> p.name})
endfunction
