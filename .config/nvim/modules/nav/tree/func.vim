command! -nargs=0 -bar DfFindInTree call s:sync_nerdtree()
command! -nargs=0 -bar DfSyncTree call s:sync_nerdtree_cwd()

function! s:sync_nerdtree()
  if winnr('$') > 1
    call s:sync_tree()
  endif
endfunction

function! s:sync_nerdtree_cwd()
  if winnr('$') > 1
    call s:sync_cwd()
  endif
endfunction

function! s:sync_tree()
  let s:curwnum = winnr()
  NERDTreeFind
  exec s:curwnum . 'wincmd w'
endfunction

function! s:sync_cwd()
  let s:curwnum = winnr()
  execute 'lcd ' . projectroot#guess()
  NERDTreeCWD
  exec s:curwnum . 'wincmd w'
endfunction
