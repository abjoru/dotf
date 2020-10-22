let s:LOG = DotF#logger#derive('modules')

let s:mod_dir = expand(resolve($HOME . '/.config/nvim/modules'))

let s:self = {
  \ 'dir': '',
  \ 'all': [],
  \ 'enabled': [],
  \ 'core': [],
  \ 'plugins': [],
  \ 'module_plugins': []
  \ }

function! DotF#api#modules#get() abort
  let s:self.dir = s:mod_dir
  let s:self.all = s:scan_modules()
  let s:self.core = ['core/base']
  return deepcopy(s:self)
endfunction

function! s:self.add_module(name) abort
  call s:LOG.info('Added module: ' . a:name)
  if index(self.enabled, a:name) ==? -1
    call add(self.enabled, a:name)
  endif
endfunction

function! s:self.add_plugin(name, ...) abort
  let l:cfg = get(a:, '1', {})
  if index(self.plugins, a:name) ==? -1
    call add(self.plugins, {'name': a:name, 'config': l:cfg})
  endif
endfunction

function! s:self.add_module_plugin(name, ...) abort
  let l:cfg = get(a:, '1', {})
  if index(self.module_plugins, a:name) ==? -1
    call add(self.module_plugins, {'name': a:name, 'config': l:cfg})
  endif
endfunction

function! s:self.list_modules() abort
  return self.all
endfunction

function! s:self.list_enabled_modules() abort
  return self.enabled
endfunction

function! s:self.list_core_modules() abort 
  return self.core
endfunction

function! s:self.list_enabled_core_modules() abort
  let l:cores = []
  for l:module in self.enabled
    if index(self.core, l:module) != -1
      call add(l:cores, l:module)
    endif
  endfor

  return l:cores
endfunction

function! s:self.list_enabled_noncore_modules() abort
  let l:modules = []
  for l:module in self.enabled
    if index(self.core, l:module) == -1
      call add(l:modules, l:module)
    endif
  endfor

  return l:modules
endfunction

function! s:self.list_plugins() abort
  " combine module_plugins and plugins and make it distinct
  let l:pgs = self.module_plugins
  for l:plugin in self.plugins
    if index(l:pgs, l:plugin) == -1
      call add(l:pgs, l:plugin)
    endif
  endfor

  return l:pgs
endfunction

function! s:self.get_module_dir() abort
  return self.dir
endfunction

function! s:scan_modules() abort
  let l:located_modules = []
  for l:group in split(glob(s:mod_dir . '/*'), '\n')
    for l:module in split(glob(l:group . '/*'), '\n')
      if filereadable(l:module . '/config.vim') || filereadable(l:module . '/packages.vim')
        let l:module_name = substitute(l:module, s:mod_dir . '/', '', '')
        call add(l:located_modules, l:module_name)
        call s:LOG.warn('Found ' . l:module_name)
      endif
    endfor
  endfor

  return l:located_modules
endfunction
