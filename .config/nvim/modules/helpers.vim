" Helper functions for creating modules

"""""""
" API "
"""""""

command! -nargs=+ -bar DfAddPlugin call DotfAddPlugin(<args>)
command! -nargs=+ -bar DfSpaceIndent call DotfSetFTIndentation(<args>)
command! -nargs=+ -bar DfTabsIndent call DotfSetFTTabsIndentation(<args>)
command! -nargs=1 -bar DfCleanupFileTypeGroups call DotfCleanupFileTypeGroups(<args>)
command! -nargs=+ -bar DfLoadFunc call DotfLoadFunc(<args>)

function! DotfAddPlugin(name, ...)
  let l:config = get(a:, '1', {})
  call dotf#plugin(a:name, l:config)
endfunction

function! DotfSetFTIndentation(ft, indentation)
  let l:indent = get(g:, 'df_' . a:ft . '_indentation', a:indentation)
  execute 'au FileType ' . a:ft . ' setlocal expandtab shiftwidth=' . l:indent . ' tabstop=' . l:indent
endfunction

function! DotfSetFTTabsIndentation(ft, indentation)
  let l:indent = get(g:, 'df_' . a:ft . '_indentation', a:indentation)
  execute 'au FileType ' . a:ft . ' setlocal noexpandtab shiftwidth=' . l:indent . ' tabstop=' . l:indent
endfunction

function! DotfCleanupFileTypeGroups(ft)
  execute 'au BufLeave * if &ft ==# "' . a:ft . '" | let g:lmap.m = { "name": "major-mode-cmd" } | endif '
endfunction

function! DotfLoadFunc(spath, ...)
  let l:script_name = 'func.vim'
  if a:0 > 0
    let l:script_name = a:1
  endif

  let l:path = fnamemodify(resolve(a:spath), ':h')
  execute 'source ' . l:path . '/' . l:script_name
endfunction

""""""""""""""""
" Internal API "
""""""""""""""""

function! DotfIsModuleEnabled(name)
  if !exists('g:dotf_enabled_modules')
    return 1
  endif

  if index(g:dotf_enabled_modules, a:name) != 1
    return 1
  endif
endfunction
