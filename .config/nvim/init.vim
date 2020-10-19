" -*- mode: vimrc -*-
" vim: ft=vim

function! Modules()
  " Enabled Modules
  DfModule 'core/base'
  DfModule 'ui/statusbar'

  DfModule 'nav/tree'
  DfModule 'nav/splash'
  DfModule 'nav/buffers'
  DfModule 'nav/search'
  DfModule 'ui/colors'
  DfModule 'tools/terminal'
  DfModule 'tools/completion'
  DfModule 'tools/drag'

  DfModule 'ui/icons'

  " Extra Plugins
  DfPlugin 'morhetz/gruvbox'
endfunction

function! Options()
  DfTreeWidth(40)
endfunction

function! DotfPre()
endfunction

function! DotfPost()
  DfSetTheme 'dark', 'gruvbox', 'gruvbox'
  DfStartWithTree
endfunction

" Loader, do not edit!
call dotf#init()
call Options()
call Modules()
call DotfPre()
call dotf#bootstrap()
call DotfPost()
