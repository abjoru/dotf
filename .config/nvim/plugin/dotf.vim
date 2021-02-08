" VARS
let g:DotfTreeBeforeGlyphPadding = ' '
let g:DotfTreeAfterGlyphPadding = ' '
let g:DotfTreeGitForceAlign = 1
let g:DotfEnableTreeRedraw = 1

"""""""""""""""""""""""""""""""
" Nice-to-have debug mappings "
"""""""""""""""""""""""""""""""

" Show highlighting group for word under cursor (syntax debugging)
map <F5> :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
\ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
\ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>

" Show syntax stack under cursor
nmap <F6> :call <SID>SynStack()<CR>

function! <SID>SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

function s:get_icon(id)
  return luaeval('require("dotf/ui/devicons").get_file_icon(_A)', a:id)
endfunction

" NERDTree listener
function! NERDTreeIconDecoratorListener(event)
  let path = a:event.subject
  let prePadding = g:DotfTreeBeforeGlyphPadding
  let postPadding = g:DotfTreeAfterGlyphPadding
  let hasGitFlags = (len(path.flagSet._flagsForScope('git')) > 0)
  let hasGitNerdTreePlugin = (exists('g:loaded_nerdtree_git_status') == 1)

  "echom luaeval('require("dotf/ui/devicons").test(_A)', path)

  if g:DotfTreeGitForceAlign && !hasGitFlags && hasGitNerdTreePlugin
    let prePadding .= ' '
  endif

  if !path.isDirectory
    let flag = prePadding . s:get_icon(path.str()) . postPadding
  elseif path.isDirectory
    let directoryOpened = 0

    if len(path.flagSet._flagsForScope('webdevicons')) > 0
      if has_key(path, 'isOpen') && path.isOpen == 1
        let directoryOpened = 1
      endif
    endif

    if directoryOpened
      let flag = prePadding . luaeval('require("dotf/ui/devicons").get_dir_icon(true, false)') . postPadding
    else
      if path.isSymLink
        let flag = prePadding . luaeval('require("dotf/ui/devicons").get_dir_icon(false, true)') . postPadding
      else
        let flag = prePadding . luaeval('require("dotf/ui/devicons").get_dir_icon(false, false)') . postPadding
      endif
    endif
  else
    let flag = ''
  endif

  call path.flagSet.clearFlags('webdevicons')

  if flag !=? ''
    call path.flagSet.addFlag('webdevicons', flag)
  endif
endfunction

augroup conceal_nerdtree_brackets
  au!
  autocmd FileType nerdtree syntax match hideBracketsInNerdTree "\]" contained conceal containedin=ALL
  autocmd FileType nerdtree syntax match hideBracketsInNerdTree "\[" contained conceal containedin=ALL
  autocmd FileType nerdtree setlocal conceallevel=3
  autocmd FileType nerdtree setlocal concealcursor=nvic
augroup END
