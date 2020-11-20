
""""""""""""""""""""""""""""
" DotF Theme Configuration "
""""""""""""""""""""""""""""

""
" Sets the vim theme (and optionally airline theme if enabled)
function! DotF#themes#set(bg, name, ...) abort
  " Set background
  if a:bg ==? 'light'
    set background=light
  else
    set background=dark
  endif

  try
    " Set vim theme
    if has('termguicolors')
      set termguicolors
    endif

    " Set color scheme
    execute 'colorscheme ' . a:name
    hi Comment cterm=italic

    " Set Guilde theme
    let p = s:gruvbox_palette()
    exe 'hi! LeaderGuiderPrompt ctermbg=' . p[0][2] . ' ctermfg=' . p[0][3] . ' cterm=bold gui=bold guifg=' . p[0][0] . ' guibg=' . p[0][1]
    exe 'hi! LeaderGuiderSep1 ctermbg=' . p[1][2] . ' ctermfg=' . p[0][2] . ' cterm=bold gui=bold guifg=' . p[0][1] . ' guibg=' . p[1][1]
    exe 'hi! LeaderGuiderName ctermbg=' . p[1][2] . ' ctermfg=' . p[1][3] . ' cterm=bold gui=bold guifg=' . p[1][0] . ' guibg=' . p[1][1]
    exe 'hi! LeaderGuiderSep2 ctermbg=' . p[2][2] . ' ctermfg=' . p[1][2] . ' cterm=bold gui=bold guifg=' . p[1][1] . ' guibg=' . p[2][1]
    exe 'hi! LeaderGuiderFill ctermbg=' . p[2][2] . ' ctermfg=' . p[2][3] . ' guifg=' . p[2][0] . ' guibg=' . p[2][1]

    " Set airline theme if available
    if a:0 ==? 1
      let g:airline_theme = a:1
    endif

    " attempt to refresh airline
    if exists(':AirlineRefresh')
      :AirlineRefresh
    endif
  catch
  endtry
endfunction

function! s:gruvbox_palette() abort
  return [
    \ ['#282828', '#a89984', 246, 235],
    \ ['#a89984', '#504945', 239, 246],
    \ ['#a89984', '#3c3836', 237, 246],
    \ ['#665c54', 241],
    \ ['#282828', '#83a598', 235, 109],
    \ ['#282828', '#fe8019', 235, 208],
    \ ['#282828', '#8ec07c', 235, 108],
    \ ['#282828', '#689d6a', 235, 72],
    \ ['#282828', '#8f3f71', 235, 132],
    \ ]
endfunction
