command! -bang -nargs=? -complete=dir Files
      \ call fzf#vim#files(<q-args>, fzf#vim#with_preview(), <bang>0)

if executable('rg')
  function! s:rg(query, bang)
    let l:query = a:query
    if empty(a:query)
      call inputsave()
      let l:query = input('Query: ')
      call inputrestore()
    endif

    call fzf#vim#grep(
          \'rg --column --line-number --no-heading --color-always '.shellescape(l:query),
          \1,
          \a:bang ? fzf#vim#with_preview('up:60%')
          \       : fzf#vim#with_preview('right:50%:hidden', '?'),
          \a:bang)
  endfunction

  command! -bang -nargs=* Rg call s:rg(<q-args>, <bang>0)
endif

if DotfIsModuleEnabled('scm/git')
  function! s:git_grep(query, bang)
    let l:query = a:query
    if empty(a:query)
      call inputsave()
      let l:query = input('Query: ')
      call inputrestore()
    endif

    call fzf#vim#grep('git grep --line-number '.shellescape(l:query), 0, a:bang)
  endfunction

  command! -bang -nargs=* GGrep call s:git_grep(<q-args>, <bang>0)
endif
