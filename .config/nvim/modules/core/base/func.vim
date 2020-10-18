" Use ripgrep if available
if executable('rg')
  if !DotfIsModuleEnabled('nav/fzf')
    command! -nargs=+ -complete=file -bar Rg silent! grep! <args>|cwindow|redraw!
  endif

  set grepprg=rg\ --vimgrep
  set grepformat^=%f:%l:%c:%m
endif
