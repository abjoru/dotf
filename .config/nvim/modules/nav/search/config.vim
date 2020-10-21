let g:fzf_preview_window = 'right:60%'

let g:lmap.f = get(g:lmap, 'f', { 'name': 'files' })

DfNMap 'fb', 'find-buffer', 'Buffers'
DfNMap 'ff', 'find-file', 'Files'
DfNMap 'fl', 'find-line', 'Lines'

if executable('rg')
  DfNMap 'fg', 'find-rg', 'Rg'
endif

if DotF#modules#isenabled('scm/git')
  DfNMap 'fG', 'find-git-grep', 'GGrep'
endif
