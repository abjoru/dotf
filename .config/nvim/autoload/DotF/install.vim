let s:LOG = DotF#logger#derive('install')
let s:UTILS = DotF#api#import('utils')

function! DotF#install#run() abort
  call s:LOG.info('Starting install process...')
  let has_python = s:UTILS.has_python()

  if has_python ==? 0
    echoerr 'IMPORTANT! Neovim could not find support for python, which means'
    echoerr 'some modules may not work. To fix this, install the neovim python'
    echoerr 'package. I.e. `pip install neovim` etc'
  else
    call DotF#plug#download()
    call DotF#plug#install()
  
    call s:LOG.info('-- Installation complete, please restart Neovim! --')
    :quitall
  endif
endfunction
