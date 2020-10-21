command! -range -nargs=0 -bar DfToggleComment <line1>,<line2>call s:nerd_toggle_comment()
command! -range -nargs=0 -bar DfInvertToggleComment <line1>,<line2>call s:nerd_invert_toggle_comment()
command! -range -nargs=0 -bar DfYankAndComment <line1>,<line2>call s:nerd_yank_and_comment()
command! -range -nargs=0 -bar DfInvertYankAndComment <line1>,<line2>call s:nerd_invert_yank_and_comment()

function! s:nerd_toggle_comment()
  call NERDComment(1, 'toggle')
endfunction

function! s:nerd_invert_toggle_comment()
  call NERDComment(1, 'invert')
endfunction

function! s:nerd_yank_and_comment()
  call NERDComment(1, 'yank')
  normal! p
endfunction

function! s:nerd_invert_yank_and_comment()
  normal! y
  normal! gv
  call NERDComment(1, 'invert')
  normal! p
endfunction
