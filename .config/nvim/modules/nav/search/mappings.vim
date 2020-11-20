let g:lmap.f = get(g:lmap, 'f', { 'name': 'Files' })

DfNMap 'ff', 'Find file', 'Files'
DfNMap 'fb', 'Find buffer', 'Buffers'
DfNMap 'fl', 'Find line', 'Lines'

if executable('rg')
  DfNMap 'fg', 'RipGrep', 'Rg'
endif

if DotF#modules#is_enabled('scm/git')
  DfNMap 'fG', 'GitGrep', 'GGrep'
endif
