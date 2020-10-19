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
  " Tree width
  let g:dotf_nav_tree_width = 40
  " Tab support
  let g:dotf_ui_statusbar_tabline_enabled = 1
endfunction

function! DotfPre()
  "EnableDebug
  "EnableVerboseDebug
  "EnableModuleDebug
endfunction

function! DotfPost()
  DfSetTheme 'dark', 'gruvbox', 'gruvbox'

  "if exists('g:loaded_webdevicons')
    "call webdevicons#refresh()
  "endif
endfunction

" Loader, do not edit!
call dotf#init()
call Options()
call Modules()
call DotfPre()
call dotf#bootstrap()
call DotfPost()
