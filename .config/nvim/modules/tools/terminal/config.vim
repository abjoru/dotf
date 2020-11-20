" TODO make this toggle open terminal or create new one (only leaving one
" term)
"map <silent> <F3> :DfTerminalOpen<CR>
map <silent> <F3> :ToggleTerminal<CR>

" Do not switch to insert when giving focus to existing terminal
let g:auto_start_insert = 0
" Do start new terminal in insert mode
let g:open_in_insert_mode = 1

" avoid `No write since last change` messages when toggling (?)
"set autowriteall

"let g:lmap['!'] = ['DfTerminalCmd', 'shell-cmd']
"DfNMap '!', 'shell-cmd', 'DfTerminalCmd'

if has('nvim')
  " make terminal split nav behave like normal
  tnoremap <C-h> <C-\><C-N><C-w>h
  tnoremap <C-j> <C-\><C-N><C-w>j
  tnoremap <C-k> <C-\><C-N><C-w>k
  tnoremap <C-l> <C-\><C-N><C-w>l

  " automatically enter insert mode when entering terminal buffer
  augroup terminalConfig
    au!
    "au TermOpen * :startinsert
    "au BufEnter * if &buftype == 'terminal' | :startinsert | endif
    " change status line to terminal title when inside terminal
    " TODO find way to change terminal buffer title permanently
    au TermOpen * setlocal statusline=%{b:term_title}
    au BufEnter * if &buftype == 'terminal' | setlocal statusline=%{b:term_title} | endif
  augroup END
endif
