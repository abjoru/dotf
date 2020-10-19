let s:LOG = DotF#logger#derive('sync')

let s:cache_dir = expand(resolve($HOME . '/.cache/nvim'))
let s:cache_file = expand(resolve(s:cache_dir . '/loaded-plugins.vim'))

  function! DotF#sync#run() abort
    call s:detect_plugin_changes()
    call DotF#sync#configuration()
  endfunction
" make sure we don't re-source these functions!
if !exists('g:dotf_update_and_sync_already_defined')
  let g:dotf_update_and_sync_already_defined = 1

  function! DotF#sync#configuration() abort
    call s:LOG.info('Syncing configuration, please hold on!...')
    call s:LOG.info('  - setting dotf_postinit_loaded to 0')
    let g:dotf_postinit_loaded = 0
    call s:LOG.info('  - sourcing $MYVIMRC')
    :source $MYVIMRC
    call s:LOG.info('  - calling post initialization')
    call g:Dotf_postinit()
    call s:LOG.info('Finished configuration sync!')
  endfunction
endif

function! DotF#sync#check_plugins() abort
  if !isdirectory(s:cache_dir)
    call mkdir(s:cache_dir, 'p')
  endif

  if filereadable(s:cache_file)
    let l:old_plugins = readfile(s:cache_file)
  else
    let l:old_plugins = []
  endif

  if l:old_plugins != DotF#modules#enabledpluginnames()
    call s:LOG.warn('!Plugin changes detected! Please run :DfSync to refresh installation!')
    echohl WarningMsg
    echo 'Warning'
    echohl None
    echon ': Plugin changes detected! Please run :DfSync to refresh installation!'
  else
    call s:LOG.info('Plugins are up to date!')
  endif
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

  if l:old_plugins == DotF#modules#enabledpluginnames()
    call s:LOG.info('No changes in plugins')
  else
    call s:LOG.info('Plugins changes detected, installing...')
    call DotF#plug#install()
  endif
endfunction
