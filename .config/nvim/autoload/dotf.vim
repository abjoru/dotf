" This neovim configuration is heavily influenced by SpaceNeovim/SpaceVim

"""""""""""
" Globals "
"""""""""""

" TODO deprecate this
let g:scratch_buffer_no = get(g:, 'scratch_buffer_no', -1)

" Global Paths
let g:home_dir = $HOME
let g:cache_dir = expand(resolve(g:home_dir . '/.cache/nvim'))
let g:config_dir = expand(resolve(g:home_dir . '/.config/nvim'))
let g:modules_dir = expand(resolve(g:config_dir . '/modules'))
let g:plugged_dir = expand(resolve(g:cache_dir . '/plugged'))

" Task list file
let g:dotf_tasks_file = get(g:, 'dotf_tasks_file', g:config_dir . '/dotf.todo')

" Defines leader key (default: <Space>)
let g:dotf_leader_key = get(g:, 'dotf_leader_key', '<Space>')

" NERDTree width configuration
let g:dotf_nav_tree_width = get(g:, 'dotf_nav_tree_width', 45)

" Airline tabline on/off switch
let g:dotf_ui_statusbar_tabline_enabled = get(g:, 'dotf_ui_statusbar_tabline_enabled', 1)

" Local logger instance
let s:LOG = DotF#logger#derive('bootstrap')

"""""""""""""""""
" Bootstrapping "
"""""""""""""""""

function! dotf#initialize()
  " Load DotF command API
  call DotF#commands#load()
endfunction

""
" Standard bootstrapping for DotF
function! dotf#bootstrap() abort
  let has_python = DotF#api#has_python()
  let modApiFile = g:modules_dir . '/auto-modules.vim'

  if filereadable(modApiFile)
    echo modApiFile
    execute 'source ' . modApiFile
  endif

  if has_python ==? 1 || exists('g:gui_oni')
    call s:source_modules()
    call s:load_all_plugins()
  endif

  " perform update check on startup
  augroup dotf_check_plugins
    au!
    au VimEnter * call DotF#updates#check()
  augroup END
endfunction

" Load all plugins using vim-plug
function! s:load_all_plugins() abort
  call plug#begin(g:plugged_dir)

  call s:LOG.info('Loading all plugins...')
  for plugin in DotF#modules#list_enabled_plugins()
    call s:LOG.info('Installing ' . plugin.name)
    Plug plugin.name, plugin.config
  endfor

  call plug#end()
endfunction

" Source all user defined modules
function! s:source_modules() abort
  call s:LOG.info('Loading core modules...')
  for mod in DotF#modules#list_enabled_core_modules()
    call s:source_module(mod)
  endfor

  call s:LOG.info('Loading enabled modules...')
  for mod in DotF#modules#list_enabled_noncore_modules()
    call s:source_module(mod)
  endfor
endfunction

" Sources a given module identified by `module_name`
function! s:source_module(module_name) abort
  let pkgSourceFile = g:modules_dir . '/' . a:module_name . '/packages.vim'
  let funcSourceFile = g:modules_dir . '/' . a:module_name . '/func.vim'
  let confSourceFile = g:modules_dir . '/' . a:module_name . '/config.vim'

  " Source package defintions
  if filereadable(pkgSourceFile)
    call s:LOG.info('Sourcing packages for ' . a:module_name)
    execute 'source ' . pkgSourceFile
  endif

  " Source function definitions
  if filereadable(funcSourceFile)
    call s:LOG.info('Sourcing functions for ' . a:module_name)
    execute 'source ' . funcSourceFile
  endif

  " Source config definitions
  if filereadable(confSourceFile)
    call s:LOG.info('Sourcing configurations for ' . a:module_name)
    execute 'source ' . confSourceFile
  endif
endfunction

"""""""""""""""""
" API functions "
"""""""""""""""""

""
" Get the configured leader key.
function! dotf#get_leader_key()
  return g:dotf_leader_key
endfunction

""
" Sets the leader key
function! dotf#set_leader_key(new_leader)
  let g:dotf_leader_key = a:new_leader
endfunction
