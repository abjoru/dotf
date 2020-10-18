" -*- mode: vimrc -*-
" vim: ft=vim

function! Modules()
  Module 'core/base'
  Module 'nav/tree'
  Module 'nav/splash'
  Module 'nav/buffers'
  Module 'nav/search'
  Module 'ui/statusbar'
  Module 'tools/terminal'
  Module 'tools/completion'

  Plugin 'morhetz/gruvbox'
endfunction

function! DotfPre()
  "EnableDebug
  "EnableVerboseDebug
  "EnableModuleDebug
endfunction

function! DotfPost()
  SetTheme 'dark', 'gruvbox', 'gruvbox'
endfunction

" Loader, do not edit!
call dotf#init()
call Modules()
call DotfPre()
call dotf#bootstrap()
call DotfPost()
