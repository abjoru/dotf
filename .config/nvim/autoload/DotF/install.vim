let s:LOG = DotF#logger#derive('install')
let s:UTILS = DotF#api#import('utils')

function! DotF#install#run() abort
  call s:LOG.info('Starting install process...')
  let has_python = s:UTILS.has_python()

  if has_python ==? 0
    call s:LOG.error('IMPORTANT! Neovim could not find support for python, which means')
    call s:LOG.error('some modules may not work. To fix this, install the neovim python')
    call s:LOG.error('package. I.e. `pip install neovim` etc')
  endif

  call DotF#plug#download()
  call DotF#plug#install()

  call s:LOG.info('-- Installation complete, please restart Neovim! --')
  :quitall
endfunction
