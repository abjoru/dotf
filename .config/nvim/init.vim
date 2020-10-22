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
  DfModule 'nav/comments'
  DfModule 'ui/colors'
  DfModule 'tools/terminal'
  DfModule 'tools/drag'
  DfModule 'tools/tasks'
  DfModule 'scm/git'
  DfModule 'ui/icons'
  DfModule 'ui/windows'
  DfModule 'tools/completion'

  " Extra Plugins
  DfPlugin 'morhetz/gruvbox'
endfunction

function! Options()
  DfTreeWidth(40)
endfunction

function! Before()
endfunction

function! After()
  DfSetTheme 'dark', 'gruvbox', 'gruvbox'
  DfStartWithTree
endfunction

" Loader, do not edit!
call dotf#initialize()
call Options()
call Modules()
call Before()
call dotf#bootstrap()
call After()

" Describe New Boot sequence
"
" 1. load commands
" 2. check for plugin changes (augroup?)
"   a. ask to update follow by quit (for reload)
"   b. proceed normally if no changes
" 3. load plugins
" 4. done
"
" Updates should have its own separate flow/command. This would really only
" deal with plugin/coc changes since bootstrap takes care of new modules. Not
" a problem having to restart after module addition, which would prevent
" having to re-source code anyways (safer)
