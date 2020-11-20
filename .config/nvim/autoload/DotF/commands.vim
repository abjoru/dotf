let s:LOG = DotF#logger#derive('commands')
let s:MODULES = DotF#modules#instance()

function! DotF#commands#load() abort

  """"""""""""""""""
  " Setup commands "
  """"""""""""""""""

  " Main installation command (called from install scripts)
  command! -nargs=0 -bar DfInstall call s:install_dotf()

  " Update DotF (vim plug and coc)
  command! -nargs=0 -bar DfUpdate call s:update_dotf()

  """"""""""""""""""""""""
  " User config commands "
  """"""""""""""""""""""""

  " Configure module
  command! -nargs=1 -bar DfModule call s:MODULES.add_module(<args>)

  " Configure plugin
  command! -nargs=* -bar DfPlugin call s:MODULES.add_plugin(<args>)

  ""
  " Configure DotF themes
  " param1: background [light|dark]
  " param2: color scheme name [gruvbox|..]
  " param3: airline theme [gruvbox|..]
  command! -nargs=+ -bar DfSetTheme call DotF#themes#set(<args>)

  ""
  " Sets the initial width of the NERDTree buffer.
  " param1: int (default 45)
  command! -nargs=1 -bar DfTreeWidth call s:tree_width(<args>)

  ""
  " Disables the tabline.
  command! -nargs=0 -bar DfDisableTabline call s:disable_tabline()

  ""
  " Starts with NERDTree open.
  command! -nargs=0 -bar DfStartWithTree call s:start_with_tree()

  """"""""""""""""""""
  " Utility commands "
  """"""""""""""""""""

  ""
  " View runtime log
  command! -nargs=0 -bar DfRuntimeLog call DotF#logger#viewRuntimeLog()

  """"""""""""""""""""
  " Modules commands "
  """"""""""""""""""""

  " Use in packages.vim to add plugin requirement for module
  command! -nargs=* -bar DfAddPlugin call s:add_mplugin(<args>)

  " Use in init.vim to configure space indentation
  command! -nargs=+ -bar DfSpaceIndent call s:set_space_indentation(<args>)

  " use in init.vim to configure tab indentation
  command! -nargs=+ -bar DfTabIndent call s:set_tab_indentation(<args>)

endfunction

"""""""""""""""""""""""""""
" Command implementations "
"""""""""""""""""""""""""""

" Set nerdtree width
function! s:tree_width(width) abort
  let g:dotf_nav_tree_width = a:width
endfunction

" Disable tabline
function! s:disable_tabline() abort
  let g:dotf_ui_statusbar_tabline_enabled = 0
endfunction

" Show nerdtree on startup
function! s:start_with_tree() abort
  autocmd VimEnter * 
        \ if !argc()
        \ | Startify
        \ | NERDTree
        \ | wincmd w
        \ | endif
endfunction

" Enable plugin from module
function! s:add_mplugin(name, ...) abort
  let l:cfg = get(a:, '1', {})
  call s:MODULES.add_module_plugin(a:name, l:cfg)
endfunction

function! s:set_space_indentation(ft, indentation) abort
  let l:indent = get(g:, 'df_' . a:ft . '_indentation', a:indentation)
  execute 'au FileType ' . a:ft . ' setlocal expandtab shiftwidth=' . l:indent . ' tabstop=' . l:indent
endfunction

function! s:set_tab_indentation(ft, indentation) abort
  let l:indent = get(g:, 'df_' . a:ft . '_indentation', a:indentation)
  execute 'au FileType ' . a:ft . ' setlocal noexpandtab shiftwidth=' . l:indent . ' tabstop=' . l:indent
endfunction

function! s:install_dotf() abort
  call DotF#install#run()
endfunction

function! s:update_dotf() abort
  call DotF#updates#run()
endfunction
