let s:LIST = DotF#api#import('list')

let g:haskell = [
\'',
\'     ############(    **///////*//*                                           ',
\'     ,############.   .///*///*////.                                          ',
\'       (###########(    //////*//////                                         ',
\'        ,############.   .***********/                                        ',
\'          ############(    ///*///////**                                      ',
\'           *############    ,/*///*///*//                                     ',
\'             ############(    ////////*///*    (###########################   ',
\'              *############    .*/*/*/*/*/*/.   .##########################   ',
\'                ############(    *////*///////    #########################   ',
\'                 ,############.   .///*///*////.   ,#######################   ',
\'                   ############(    //*///////*/*                             ',
\'                   ############/    /************/                            ',
\'                 /############    ,///*///////*////*    (##################   ',
\'                ############/    /*///*///*///*///*//    ,#################   ',
\'              /############    *//////*///////*///////*    (###############   ',
\'             ############/    //*/*/*/*/*/,.*/*/*/*/*/*/.   .##############   ',
\'           /############    ,/*///////*//    /*///////*///                    ',
\'          ############(    ///*///*///**      .///*///*///*.                  ',
\'        /############    ,////*///////          //////*/////*                 ',
\'       ############/    *************            ./***********                ',
\'     /############    ,///////*////                ///*///////**              ',
\'    ############/    /*///*///*//,                  ./*///*///*//.            ',
\'',
\]

let g:startify_custom_header = g:haskell "+ startify#fortune#boxed()

" returns all modified files of the current git repo
" `2>/dev/null` makes the command fail quietly if not in repo
function! s:git_modified()
  let l:files = []
  if getcwd() == shellescape(fnamemodify('~', ':p'))
    let l:files = systemlist('dotf ls-files -m 2>/dev/null')
  else
    let l:files = systemlist('git ls-files -m 2>/dev/null')
  endif

  return map(l:files, {v -> {'line': v, 'path': v}})
endfunction

" same as above but shows untracked files (not for dotf)
function! s:git_untracked()
  let l:files = systemlist('git ls-files -o --exclude-standard 2>/dev/null')
  return map(l:files, {v -> {'line': v, 'path': v}})
endfunction

function! s:load_tasks(file)
  let l:output = []
  if DotF#modules#isenabled('tools/tasks') && filereadable(a:file)
    let l:lines = readfile(a:file)
    let l:topic = ''
    let l:tasks = []

    for l:line in filter(l:lines, {_, l -> !empty(l)})
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
    endfor

    if !empty(l:topic) && !empty(l:tasks)
      call add(l:output, {'topic': l:topic, 'tasks': l:tasks})
    endif
  endif

  return l:output
endfunction

function! s:tasks(index)
  let l:data = s:load_tasks($HOME . '/.config/nvim/dotf.todo')
  return l:data[a:index].tasks
endfunction

function! s:build_startify_lists()
  let l:startify = [
    \ { 'type': 'files', 'header': ['   MRU'] },
    \ { 'type': 'dir', 'header': ['   MRU ' . getcwd()] },
    \ { 'type': {-> s:git_modified()}, 'header': ['   GIT Modified'] },
    \ { 'type': {-> s:git_untracked()}, 'header': ['   GIT Untracked'] }
    \ ]

  let l:tasks = s:load_tasks($HOME . '/.config/nvim/dotf.todo')
  let l:tasks = map(l:tasks, {i, v -> {'idx': i, 'topic': v.topic}})

  for item in l:tasks
    call add(l:startify, {'type': {-> s:tasks(item.idx)}, 'header': ['   Tasks: ' . item.topic]})
  endfor

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
