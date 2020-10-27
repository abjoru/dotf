let s:LOG = DotF#logger#derive('cache')
let s:MODULES = DotF#modules#instance()

let s:self = {}

let s:plugin_cache_file = expand(resolve(g:cache_dir . '/loaded-plugins.vim'))

function! DotF#api#cache#get() abort
  return deepcopy(s:self)
endfunction

function! s:self.write_plugins() abort
  if !isdirectory(g:cache_dir)
    call mkdir(g:cache_dir, 'p')
  endif

  let names = s:get_plugin_names()

  if writefile(names, s:plugin_cache_file)
    call s:LOG.error('Could not write loaded plugins to cache file: ' . s:plugin_cache_file)
  else 
    call s:LOG.info('Wrote loaded plugins: ' . string(names))
  endif
endfunction

function! s:self.read_plugins() abort
  if filereadable(s:plugin_cache_file)
    let plugins = readfile(s:plugin_cache_file)
  else
    let plugins = []
  endif

  return plugins
endfunction

function! s:self.has_plugin_changes() abort
  let current = sort(s:get_plugin_names())
  let cached = sort(s:self.read_plugins())
  return cached != current
endfunction

function! s:get_plugin_names() abort
  let plugins = s:MODULES.list_plugins()
  return map(copy(plugins), {_, p -> p.name})
endfunction
