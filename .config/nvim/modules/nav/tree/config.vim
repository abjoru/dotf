" API
let g:dotf_nav_tree_width = get(g:, 'dotf_nav_tree_width', 45)

" Toggle tree with <F2>
map <silent> <F2> :NERDTreeToggle<CR>

let g:lmap.f = get(g:lmap, 'f', { 'name': 'files' })

DfNMap 'fr', 'nerdtree-reveal', 'NERDTreeFind'
DfNMap 'fs', 'save-buffer', ':w'
DfNMap 'fS', 'save-buffer', ':w'
DfNMap 'fn', 'nerdtree-sync', 'SyncNERDTree'

let g:lmap.f.e = get(g:lmap.f, 'e', { 'name': 'dotf' })

DfNMap 'fed', 'find-dotfile', 'e $MYVIMRC'
DfNMap 'fem', 'find-dotf-modules', 'e $HOME/.config/nvim/modules'
DfNMap 'feU', 'update-plugins', 'PlugUpdate!'
DfNMap 'feC', 'clean-plugins', 'PlugClean!'

" NERDTree Git
let g:NERDTreeGitStatusIndicatorMapCustom = {
\ "Modified"  : "·",
\ "Staged"    : "+",
\ "Untracked" : "*",
\ "Renamed"   : " ",
\ "Unmerged"  : " ",
\ "Deleted"   : "x",
\ "Dirty"     : "·",
\ "Clean"     : "√",
\ "Ignored"   : " ",
\ "Unknown"   : " "
\ }

let g:NERDTreeGitStatusShowIgnored = 1      " enables ignored highlighting
let g:NERDTreeGitStatusNodeColorization = 1 " enables colorization
let g:NERDTreeGitStatusWithFlags = 1        " enables flags, required by colorization

highlight link NERDTreeDir Question               " custom color
highlight link NERDTreeGitStatusIgnored Comment   " custom color
highlight link NERDTreeGitStatusModified cssURL   " custom color

" NERDTree
let g:NERDTreeDirArrowExpandable = nr2char(8200)  " sets expandable character
let g:NERDTreeDirArrowCollapsible = nr2char(8200) " sets collapsible character
let g:WebDevIconsNerdTreeAfterGlyphPadding = ' '  " removes padding after devicon glyph
let g:WebDevIconsUnicodeDecorateFolderNodes = 1   " enables decorating folder nodes
let g:NERDTreeWinSize = g:dotf_nav_tree_width     " tree width
let NERDTreeRespectWildIgnore = 1                 " respect global wildignore

augroup NERDTreeConfig
  au!
  " if yuo show hidden characters, this hides them in NERDTree
  au FileType nerdtree setlocal nolist
augroup END
