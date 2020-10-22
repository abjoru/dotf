function! s:git_modified() abort
  let files = systemlist('git ls-files -m 2>/dev/null')
  return map(files, {v -> {'line': v, 'path': v}})
endfunction

function! s:git_untracked() abort
  let files = systemlist('git ls-files -o --exclude-standard 2>/dev/null')
  return map(files, {v -> {'line': v, 'path': v}})
endfunction

function! s:load_tasks(file) abort
  let output = []

  if DotF#modules#is_enabled('tools/tasks') && filereadable(a:file)
    let lines = readfile(a:file)
    let topic = ''
    let tasks = []

    for line in filter(lines, {_, l -> !empty(l)})
      if line =~ ".*:\s*$"
        if !empty(topic)
          call add(output, {'topic': topic, 'tasks': tasks})
          let topic = substitute(line, ':', '', '')
          let tasks = []
        else
          let topic = substitute(line, ':', '', '')
        endif
      else
        call add(tasks, {'line': line, 'path': a:file})
      endif
    endfor

    if !empty(topic) && !empty(tasks)
      call add(output, {'topic': topic, 'tasks': tasks})
    endif
  endif

  return output
endfunction

function! s:tasks(index) abort
  let data = s:load_tasks(g:dotf_tasks_file)
  return data[a:index].tasks
endfunction

function! g:Dotf_build_startify_lists() abort
  let output = [
    \ { 'type': 'files', 'header': ['   MRU'] },
    \ { 'type': 'dir', 'header': ['   MRU ' . getcwd()] },
    \ { 'type': {-> s:git_modified()}, 'header': ['   GIT Modified'] },
    \ { 'type': {-> s:git_untracked()}, 'header': ['   GIT Untracked'] }
    \ ]

  let tasks = map(s:load_tasks(g:dotf_tasks_file), {i, v -> {'idx': i, 'topic': v.topic}})

  for item in tasks
    call add(output, {'type': {-> s:tasks(item.idx)}, 'header': ['   Tasks: ' . item.topic]})
  endfor

  return output
endfunction
