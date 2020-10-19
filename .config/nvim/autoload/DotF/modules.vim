let s:LOG = DotF#logger#derive('modules')
let s:MODULES = DotF#api#import('modules')

let s:dotf_dir_plugged = expand(resolve($HOME . '/.config/nvim/plugged'))

" Enable a given module
function! DotF#modules#enable(name) abort
  call s:MODULES.add_module(a:name)
endfunction

" Add a given plugin (non-module)
function! DotF#modules#plugin(name, ...) abort
  let l:cfg = get(a:, '1', {})
  call s:MODULES.add_plugin(a:name, l:cfg)
endfunction

" Add a given plugin defined by a module
function! DotF#modules#mplugin(name, ...) abort
  let l:cfg = get(a:, '1', {})
  call s:MODULES.add_module_plugin(a:name, l:cfg)
endfunction

" List all enabled plugins (module defined + additional)
function! DotF#modules#enabledplugins() abort
  return s:MODULES.list_plugins()
endfunction

function! DotF#modules#enabledpluginnames() abort
  return s:MODULES.list_plugin_names()
endfunction

" List all enabled modules
function! DotF#modules#enabledmodules() abort
  return s:MODULES.list_enabled_modules()
endfunction

" Tests if a named module has been enabled
function! DotF#modules#isenabled(name) abort
  if index(s:MODULES.list_enabled_modules(), a:name) != 1
    return 1
  endif
endfunction

" Install all modules and plugins
function! DotF#modules#install() abort
  call plug#begin(s:dotf_dir_plugged)

  call s:LOG.info('Loading core modules')
  for l:core in s:MODULES.list_enabled_core_modules()
    call s:source_module(l:core)
  endfor

  call s:LOG.info('Loading main modules: ' . string(s:MODULES.list_enabled_noncore_modules()))
  for l:main in s:MODULES.list_enabled_noncore_modules()
    call s:source_module(l:main)
  endfor

  call s:LOG.info('Loading additional plugins')
  for l:plugin in s:MODULES.list_plugins()
    call s:LOG.info('Installing ' . l:plugin.name)
    Plug l:plugin.name, l:plugin.config
  endfor

  call plug#end()
endfunction

function! s:source_module(name) abort
  if filereadable(s:MODULES.get_module_dir() . '/' . a:name . '/packages.vim')
    call s:LOG.info('Sourcing ' . a:name . '/packages.vim...')
    execute 'source ' . s:MODULES.get_module_dir() . '/' . a:name . '/packages.vim'
  endif

  if filereadable(s:MODULES.get_module_dir() . '/' . a:name . '/func.vim')
    call s:LOG.info('Sourcing ' . a:name . '/func.vim...')
    execute 'source ' . s:MODULES.get_module_dir() . '/' . a:name . '/func.vim'
  endif

  if filereadable(s:MODULES.get_module_dir() . '/' . a:name . '/config.vim')
    call s:LOG.info('Sourcing ' . a:name . '/config.vim...')
    execute 'source ' . s:MODULES.get_module_dir() . '/' . a:name . '/config.vim'
  endif
endfunction
