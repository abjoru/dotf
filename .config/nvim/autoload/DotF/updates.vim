let s:LOG = DotF#logger#derive('update')
let s:CACHE = DotF#api#import('cache')

function! DotF#updates#run() abort
  call s:LOG.info('Starting update process...')
  call DotF#plug#install()
  :CocUpdate
endfunction

function! DotF#updates#check() abort
  if s:CACHE.has_plugin_changes()
    let choice = input('Update plugins (y/N)? ')
    if choice == 'y' || 'Y'
      call DotF#updates#run()
    endif
  else
    call s:LOG.info('No plugin changes detected!')
  endif
endfunction
