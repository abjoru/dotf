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

""
" Initializes DotF command API
function! dotf#init()
  call DotF#commands#load()
endfunction

""
" Standard bootstrapping for DotF
function! dotf#bootstrap() abort
  let l:has_python = DotF#api#has_python()

  if !exists('g:dotf_do_not_run_bootstrap')
    if filereadable(g:modules_dir . '/auto-modules.vim')
      execute 'source ' . g:modules_dir . '/auto-modules.vim'
    endif

    if l:has_python ==? 1 || exists('g:gui_oni')
      call DotF#modules#install()
    endif
  endif

  augroup dotf_check_plugins
    au!
    au VimEnter * call DotF#sync#check_plugins()
  augroup END
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
