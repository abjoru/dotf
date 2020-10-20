"let g:startify_custom_header = get(g:, 'startify_custom_header', [
"\'',
"\'          [D o t F - N e o V i m - B u i l d]',
"\'',
"\])

" returns all modified files of the current git repo
" `2>/dev/null` makes the command fail quietly if not in repo
function! s:git_modified()
  let l:files = []
  if getcwd() == shellescape(fnamemodify('~', ':p'))
    let l:files = systemlist('dotf ls-files -m 2>/dev/null')
  else
    let l:files = systemlist('git ls-files -m 2>/dev/null')
  endif
  return map(l:files, "{'line': v:val, 'path': v:val}")
endfunction

" same as above but shows untracked files (not for dotf)
function! s:git_untracked()
  let l:files = systemlist('git ls-files -o --exclude-standard 2>/dev/null')
  return map(l:files, "{'line': v:val, 'path': v:val}")
endfunction

function! s:load_tasks(file)
  let l:output = []
  if DotF#modules#isenabled('tools/tasks') && filereadable(a:file)
    let l:lines = readfile(a:file)
    let l:topic = ''
    let l:tasks = []

    for l:line in l:lines
      if !empty(l:line)
        if l:line =~ ".*:\s*$"
          if !empty(l:topic)
            call add(l:output, {'topic': l:topic, 'tasks': l:tasks})
            let l:topic = substitute(l:line, ':', '', '')
            let l:tasks = []
          else
            let l:topic = substitute(l:line, ':', '', '')
          endif
        else
          call add(l:tasks, {'line': l:line, 'path': a:file})
        endif
      endif
    endfor

    if !empty(l:topic) && !empty(l:tasks)
      call add(l:output, {'topic': l:topic, 'tasks': l:tasks})
    endif
  endif

  return l:output
endfunction

function! s:task_headers(index, data)
  return a:data[a:index].topic
endfunction

function! s:task_contents(index)
  let l:data = s:load_tasks($HOME . '/.config/nvim/dotf.todo')
  return l:data[a:index].tasks
endfunction

function! s:build_startify_lists()
  let l:startify = [
    \ { 'type': 'files', 'header': ['   MRU'] },
    \ { 'type': 'dir', 'header': ['   MRU ' . getcwd()] },
    \ { 'type': function('s:git_modified'), 'header': ['   GIT Modified'] },
    \ { 'type': function('s:git_untracked'), 'header': ['   GIT Untracked'] }
    \ ]

  let l:tasks = s:load_tasks($HOME . '/.config/nvim/dotf.todo')

  let i = 0
  let m = len(l:tasks)
  while i < m
    call add(l:startify, {'type': function('s:task_contents', [i]), 'header': ['   Tasks: ' . s:task_headers(i, l:tasks)]})
    let i += 1
  endwhile

  return l:startify
endfunction

function! s:split_tasks(lines)
  let out = []
  for line in a:lines
    if !empty(line)
      if line !~ ".*:$"
        call add(out, {'line': line, 'path': $HOME . '/.config/nvim/dotf.todo'})
      endif
    endif
  endfor

  return out
endfunction

" Don't change to directory of file opened from startify
let g:startify_change_to_dir = 0

let g:startify_lists = s:build_startify_lists()

augroup fixStartify
  au!
  au User Startified nmap <buffer> k k
  au User Startified nmap <buffer> j j
  au User Startified nmap <buffer> q :q<CR>
augroup END
