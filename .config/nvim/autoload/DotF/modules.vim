let s:LOG = DotF#logger#derive('modules')
let s:MODULES = DotF#api#import('modules')

let s:dotf_dir_plugged = expand(resolve($HOME . '/.config/nvim/plugged'))

" Enable a given module
function! DotF#modules#module(name) abort
  call s:MODULES.add_module(a:name)
endfunction

" Add a given plugin (non-module)
function! DotF#modules#plugin(name, ...) abort
  let l:cfg = get(a:, '1', {})
  call s:MODULES.add_plugin(a:name, l:cfg)
endfunction

" Add a given plugin defined by a module
function! DotF#modules#module_plugin(name, ...) abort
  let l:cfg = get(a:, '1', {})
  call s:MODULES.add_module_plugin(a:name, l:cfg)
endfunction

" List all enabled plugins (module defined + additional)
function! DotF#modules#list_enabled_plugins() abort
  return s:MODULES.list_plugins()
endfunction

"function! DotF#modules#list_enabled_plugin_names() abort
  "return s:MODULES.list_plugin_names()
"endfunction

" List all enabled modules
function! DotF#modules#list_enabled_modules() abort
  return s:MODULES.list_enabled_modules()
endfunction

function! DotF#modules#list_enabled_core_modules() abort
  return s:MODULES.list_enabled_core_modules()
endfunction

function! DotF#modules#list_enabled_noncore_modules() abort
  return s:MODULES.list_enabled_noncore_modules()
endfunction

" Tests if a named module has been enabled
function! DotF#modules#is_enabled(name) abort
  if index(s:MODULES.list_enabled_modules(), a:name) != 1
    return 1
  endif
endfunction
