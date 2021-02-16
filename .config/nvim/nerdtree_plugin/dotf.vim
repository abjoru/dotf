function! DirOpen(node)
  let a:node.path.isOpen = 1
  "let glyph = g:DotfTreeDirOpenSymbol
  let glyph = luaeval('require("dotf/ui/devicons").get_dir_icon(true, false)')
  call DirUpdateFlags(a:node, glyph)
endfunction

function! DirClose(node)
  let a:node.path.isOpen = 0
  "let glyph = g:DotfTreeDirClosedSymbol
  let glyph = luaeval('require("dotf/ui/devicons").get_dir_icon(false, false)')
  call DirUpdateFlags(a:node, glyph)
endfunction

function! DirUpdateFlags(node, glyph)
  let path = a:node.path
  let isOpen = a:node.isOpen
  let postPadding = g:DotfTreeAfterGlyphPadding
  let prePadding = g:DotfTreeBeforeGlyphPadding
  let hasGitFlags = (len(path.flagSet._flagsForScope('git')) > 0)
  let hasGitNerdTreePlugin = (exists('g:loaded_nerdtree_git_status') == 1)
  let collapsesToSameLine = 1 " switch this if reversed
  let dirHasOnlyOneChildDir = 0

  if collapseToSameLine
    call a:node._initChildren(1)
    let dirHasOnlyOneChildDir = (a:node.getChildCount() ==# 1 && a:node.children[0].path.isDirectory)
  endif
  
  if collapseToSameLine && dirHasOnlyOneChildDir
    call DirOpen(a:node.children[0])
  endif

  if g:DotfTreeGitForceAlign && !hasGitFlags && hasGitNerdTreePlugin
    let prePadding .= '  '
  endif

  let flag = prePadding . a:glyph . postPadding

  call a:node.path.flagSet.clearFlags('webdevicons')

  if flag !=? ''
    call a:node.path.flagSet.addFlag('webdevicons', flag)
    call a:node.path.refreshFlags(b:NERDTree)
  endif
endfunction

function! DirOpenRecursively(node)
  call DirOpen(a:node)
  for i in a:node.children
    if i.path.isDirectory ==# 1
      call DirOpenRecursively(i)
    endif
  endfor
endfunction

function! DirCloseChildren(node)
  for i in a:node.children
    if i.path.isDirectory ==# 1
      call DirClose(i)
    endif
  endfor
endfunction

function! MapActivateNode(node)
  let isOpen = a:node.isOpen
  let glyph = luaeval('require("dotf/ui/devicons").get_dir_icon(true, false)')
  let a:node.path.isOpen = !isOpen
  call DirUpdateFlags(a:node, glyph)
  call a:node.activate()

  if g:DotfEnableTreeRedraw ==# 1
    redraw!
  endif
endfunction

function! MapCloseChildren(node)
  call DirCloseChildren(a:node)
  call a:node.closeChildren()
  call b:NERDTree.render()
  call a:node.putCursorHere(0, 0)

  if g:DotfEnableTreeRedraw ==# 1
    redraw!
  endif
endfunction

function! MapCloseDir(node)
  let parent = a:node.parent
  while g:NERDTreeCascadeOpenSingleChildDir && !parent.isRoot()
    let childNodes = parent.getVisibleChildren()
    if len(childNodes) == 1 && childNodes[0].path.isDirectory
      let parent = parent.parent
    else
      break
    endif
  endwhile

  if parent ==# {} || parent.isRoot()
    call nerdtree#echo("cannot close tree root")
  else
    call parent.close()
    call DirClose(parent)
    call b:NERDTree.render()
    call parent.putCursorHere(0, 0)

    if g:DotfEnableTreeRedraw ==# 1
      redraw!
    endif
  endif
endfunction

function! MapOpenRecursively(node)
  call nerdtree#echo("Recursively opening node. Please wait...")
  call a:node.openRecursively()
  call DirOpenRecursively(a:node)
  call b:NERDTree.render()

  if g:DotfEnableTreeRedraw ==# 1
    redraw!
  endif

  call nerdtree#echo("Recursively opening node. Please wait... DONE")
endfunction

function! MapUpdirKeepOpen()
  call DirOpen(b:NERDTree.root)
  call nerdtree#ui_glue#upDir(1)
  call s:Refresh()
  
  if g:DotfEnableTreeRedraw ==# 1
    redraw!
  endif
endfunction

function! s:setup_listeners()
  call g:NERDTreePathNotifier.AddListener('init', 'NERDTreeIconDecoratorListener')
  call g:NERDTreePathNotifier.AddListener('refresh', 'NERDTreeIconDecoratorListener')
  call g:NERDTreePathNotifier.AddListener('refreshFlags', 'NERDTreeIconDecoratorListener')
endfunction

call s:setup_listeners()

call NERDTreeAddKeyMap({
      \ 'key': g:NERDTreeMapActivateNode,
      \ 'callback': 'MapActivateNode',
      \ 'override': 1,
      \ 'scope': 'DirNode'})

call NERDTreeAddKeyMap({
      \ 'key': g:NERDTreeMapOpenRecursively,
      \ 'callback': 'MapOpenRecursively',
      \ 'override': 1,
      \ 'scope': 'DirNode'})

call NERDTreeAddKeyMap({
      \ 'key': g:NERDTreeMapCloseChildren,
      \ 'callback': 'MapCloseChildren',
      \ 'override': 1,
      \ 'scope': 'DirNode'})

call NERDTreeAddKeyMap({
      \ 'key': g:NERDTreeMapCloseDir,
      \ 'callback': 'MapCloseDir',
      \ 'override': 1,
      \ 'scope': 'DirNode'})

call NERDTreeAddKeyMap({
      \ 'key': '<2-LeftMouse>',
      \ 'callback': 'MapActivateNode',
      \ 'override': 1,
      \ 'scope': 'DirNode'})

call NERDTreeAddKeyMap({
      \ 'key': '<LeftRelease>',
      \ 'callback': 'MapActivateNode',
      \ 'override': 1,
      \ 'scope': 'DirNode'})

call NERDTreeAddKeyMap({
      \ 'key': g:NERDTreeMapUpdirKeepOpen,
      \ 'callback': 'MapUpdirKeepOpen',
      \ 'override': 1,
      \ 'scope': 'all'})
