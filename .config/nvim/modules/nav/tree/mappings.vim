" Toggle tree with <F2>
map <silent> <F2> :NERDTreeToggle<CR>

let g:lmap.f = get(g:lmap, 'f', { 'name': 'Files' })

DfNMap 'fr', 'Tree reveal', 'NERDTreeFind'
DfNMap 'fs', 'Save buffer', ':w'
DfNMap 'fn', 'Tree sync', 'SyncNERDTree'

let g:lmap.f.e = get(g:lmap.f, 'e', { 'name': 'dotf' })

DfNMap 'fed', 'Find dotfile', 'e $MYVIMRC'
DfNMap 'fem', 'Find dotf-modules', 'e $HOME/.config/nvim/modules'
DfNMap 'feU', 'Update plugins', 'PlugUpdate!'
DfNMap 'feC', 'Clean plugins', 'PlugClean!'
