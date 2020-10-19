let s:LOG = DotF#logger#derive('commands')

function! DotF#commands#load() abort

  """"""""""""""""""
  " Setup commands "
  """"""""""""""""""

  " Main installation command (called from install scripts)
  command! -nargs=0 -bar DfInstall call s:install_dotf()

  " Sync plugins and configurations
  command! -nargs=0 -bar DfSync call s:syn_dotf()

  """"""""""""""""""""""""
  " User config commands "
  """"""""""""""""""""""""

  " Configure module
  command! -nargs=1 -bar DfModule call DotF#modules#enable(<args>)

  " Configure plugin
  command! -nargs=* -bar DfPlugin call DotF#modules#plugin(<args>)

  ""
  " Configure DotF themes
  " param1: background [light|dark]
  " param2: color scheme name [gruvbox|..]
  " param3: airline theme [gruvbox|..]
  command! -nargs=+ -bar DfSetTheme call DotF#themes#set(<args>)

  command! -nargs=1 -bar DfTreeWidth call s:tree_width(<args>)
  command! -nargs=0 -bar DfDisableTabline call s:disable_tabline()
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
  command! -nargs=* -bar DfAddPlugin call s:add_plugin(<args>)

  " Use in init.vim to configure space indentation
  command! -nargs=+ -bar DfSpaceIndent call s:set_space_indentation(<args>)

  " use in init.vim to configure tab indentation
  command! -nargs=+ -bar DfTabIndent call s:set_tab_indentation(<args>)

  command! -nargs=+ -bar DfBind call s:buf_bind(<args>)
  command! -nargs=+ -bar DfMap call s:buf_map(<args>)
  command! -nargs=+ -bar DfNMap call s:buf_nmap(<args>)
  command! -nargs=+ -bar DfVMap call s:buf_vmap(<args>)
  command! -nargs=+ -bar DfFileTypeBind call s:filetype_bind(<args>)
  command! -nargs=+ -bar DfFileTypeMap call s:filetype_map(<args>)
  command! -nargs=+ -bar DfFileTypeNMap call s:filetype_nmap(<args>)

endfunction

"""""""""""""""""""""""""""
" Command implementations "
"""""""""""""""""""""""""""

function! s:tree_width(width) abort
  let g:dotf_nav_tree_width = a:width
endfunction

function! s:disable_tabline() abort
  let g:dotf_ui_statusbar_tabline_enabled = 0
endfunction

function! s:start_with_tree() abort
  autocmd VimEnter * 
        \ if !argc()
        \ | Startify
        \ | NERDTree
        \ | wincmd w
        \ | endif
endfunction

function! s:add_plugin(name, ...) abort
  let l:cfg = get(a:, '1', {})
  call DotF#modules#plugin(a:name, l:cfg)
endfunction

function! s:set_space_indentation(ft, indentation) abort
  let l:indent = get(g:, 'df_' . a:ft . '_indentation', a:indentation)
  execute 'au FileType ' . a:ft . ' setlocal expandtab shiftwidth=' . l:indent . ' tabstop=' . l:indent
endfunction

function! s:set_tab_indentation(ft, indentation) abort
  let l:indent = get(g:, 'df_' . a:ft . '_indentation', a:indentation)
  execute 'au FileType ' . a:ft . ' setlocal noexpandtab shiftwidth=' . l:indent . ' tabstop=' . l:indent
endfunction

function! s:buf_bind(map, binding, name, value, isCmd) abort
  if a:isCmd
    let l:value = ':' . a:value . '<cr>'
  else
    let l:value = a:value
  endif

  if a:map ==# 'map' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'noremap'
  elseif a:map ==# 'nmap' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'nnoremap'
  elseif a:map ==# 'vmap' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'vnoremap'
  elseif a:map ==# 'tmap' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'tnoremap'
  else
    let l:noremap = ''
  endif
  
  if l:noremap !=# ''
    execute 'au VimEnter * ' . l:noremap . ' <SID>' . a:name . '# ' . l:value
    execute 'au VimEnter * ' . a:map . ' <Leader>' . a:binding . ' <SID>' . a:name . '#'
  endif
endfunction

function! s:buf_map(binding, name, value, ...) abort
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif
  call s:buf_bind('map', a:binding, a:name, a:value, l:isCmd)
endfunction

function! s:buf_nmap(binding, name, value, ...) abort
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif
  call s:buf_bind('nmap', a:binding, a:name, a:value, l:isCmd)
endfunction 

function! s:buf_vmap(binding, name, value, ...) abort
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif
  call s:buf_bind('vmap', a:binding, a:name, a:value, l:isCmd)
endfunction 

function! s:filetype_bind(ft, map, binding, name, value, isCmd) abort
  if a:isCmd
    let l:value = ':' . a:value . '<cr>'
  else
    let l:value = a:value
  endif

  if a:map ==# 'map' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'noremap'
  elseif a:map ==# 'nmap' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'nnoremap'
  elseif a:map ==# 'vmap' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'vnoremap'
  elseif a:map ==# 'tmap' && maparg('<Leader>' . a:binding, '') ==# ''
    let l:noremap = 'tnoremap'
  else
    let l:noremap = ''
  endif

  if l:noremap !=# ''
    execute 'au VimEnter * if &ft ==? "' . a:ft . '" | ' . l:noremap . ' <buffer> <SID>' . a:name . '# ' . l:value . ' | endif '
    execute 'au VimEnter * if &ft ==? "' . a:ft . '" | ' . a:map . ' <buffer> <Leader>' . a:binding . ' <SID>' . a:name . '# | endif '

    execute 'au FileType ' . a:ft . ' ' . l:noremap . ' <buffer> <SID>' . a:name . '# ' . l:value
    execute 'au FileType ' . a:ft . ' ' . a:map . ' <buffer> <Leader>' . a:binding . ' <SID>' . a:name . '#'
  endif
endfunction

function! s:filetype_map(tfile, binding, name, value, ...) abort
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif

  call s:filetype_bind(a:tfile, 'map', a:binding, a:name, a:value, l:isCmd)
endfunction

function! s:filetype_nmap(tfile, binding, name, value, ...) abort
  let l:isCmd = 1
  if a:0 > 0
    let l:isCmd = a:1
  endif

  call s:filetype_bind(a:tfile, 'nmap', a:binding, a:name, a:value, l:isCmd)
endfunction

" FIXME use local logger that logs to install_log file!
function! s:install_dotf() abort
  let l:has_python = DotF#api#has_python()

  if l:has_python ==? 0
    call s:LOG.error('IMPORTANT! Neovim could not find support for python, which means')
    call s:LOG.error('some modules may not work. To fix this, install the neovim python')
    call s:LOG.error('package. I.e. `pip install neovim` etc')
  endif

  s:LOG.info('Starting DotF installation')

  " Download and install vim-plug
  if l:has_python ==? 1 || exists('g:gui_oni')
    call DotF#plug#download()
  endif

  " source module utils
  if filereadable(g:modules_dir . '/auto-modules.vim')
    execute 'source ' . g:modules_dir . '/auto-modules.vim'
  endif

  call DotF#modules#install()
  call DotF#plug#install()

  s:LOG.info('Finished DotF installation')

  " write lock file to prevent re-installation
  if writefile([], g:config_dir . '/bootstrap_lock_file')
    call s:LOG.info('Writing installation bootstrap lock file')
  endif

  call s:LOG.info('--- Installation finished, please restart Neovim! ---')
  :quitall
endfunction

function! s:syn_dotf() abort
  let l:has_python = DotF#api#has_python()

  if filereadable(g:modules_dir . '/auto-modules.vim')
    execute 'source ' . g:modules_dir . '/auto-modules.vim'
  endif

  if l:has_python ==? 1 || exists('g:gui_oni')
    call DotF#modules#install()
  endif

  call DotF#sync#run()
endfunction
