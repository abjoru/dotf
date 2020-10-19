" TODO make this toggle open terminal or create new one (only leaving one
" term)
map <silent> <F3> :DfTerminalOpen<CR>

let g:lmap['!'] = ['DfTerminalCmd', 'shell-cmd']
DfNMap '!', 'shell-cmd', 'DfTerminalCmd'

if has('nvim')
  " make terminal split nav behave like normal
  tnoremap <C-h> <C-\><C-N><C-w>h
  tnoremap <C-j> <C-\><C-N><C-w>j
  tnoremap <C-k> <C-\><C-N><C-w>k
  tnoremap <C-l> <C-\><C-N><C-w>l

  DfNMap '-', 'terminal', 'DfTerminalOpen'

  " automatically enter insert mode when entering terminal buffer
  augroup terminalConfig
    au!
    au TermOpen * :startinsert
    au BufEnter * if &buftype == 'terminal' | :startinsert | endif
    " change status line to terminal title when inside terminal
    " TODO find way to change terminal buffer title permanently
    au TermOpen * setlocal statusline=%{b:term_title}
    au BufEnter * if &buftype == 'terminal' | setlocal statusline=%{b:term_title} | endif
  augroup END
endif
