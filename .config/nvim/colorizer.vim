augroup auto_colorize
  autocmd!
  autocmd
    \ BufNewFile,BufRead,BufEnter,BufLeave,WinEnter,WinLeave,WinNew
    \ *.hs,*.css
    \ ColorHighlight
augroup END
