"let g:startify_custom_header = get(g:, 'startify_custom_header', [
"\'',
"\'          [D o t F - N e o V i m - B u i l d]',
"\'',
"\])

" returns all modified files of the current git repo
" `2>/dev/null` makes the command fail quietly if not in repo
function! s:git_modified()
  "let l:files = []
  "if getcwd() == shellescape(fnamemodify('~', ':p'))
    "let l:files = systemlist('dotf ls-files -m 2>/dev/null')
  "else
    let l:files = systemlist('git ls-files -m 2>/dev/null')
  "endif
  return map(l:files, "{'line': v:val, 'path': v:val}")
endfunction

" same as above but shows untracked files (not for dotf)
function! s:git_untracked()
  let l:files = systemlist('git ls-files -o --exclude-standard 2>/dev/null')
  return map(l:files, "{'line': v:val, 'path': v:val}")
endfunction

let g:startify_lists = [
  \ { 'type': 'files', 'header': ['   MRU'] },
  \ { 'type': 'dir', 'header': ['   MRU ' . getcwd()] },
  \ { 'type': function('s:git_modified'), 'header': ['   GIT Modified'] },
  \ { 'type': function('s:git_untracked'), 'header': ['   GIT Untracked'] },
  \ { 'type': 'commands', 'header': ['   Commands'] }
  \ ]

augroup fixStartify
  au!
  au User Startified nmap <buffer> k k
  au User Startified nmap <buffer> j j
  au User Startified nmap <buffer> q :q<CR>
augroup END
