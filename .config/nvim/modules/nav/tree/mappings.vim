" Toggle tree with <F2>
map <silent> <F2> :NERDTreeToggle<CR>

let g:lmap.f = get(g:lmap, 'f', { 'name': 'Files' })

nnoremap <leader>fr :NERDTreeFind<CR>
let g:lmap.f.r = 'Tree reveal'

nnoremap <leader>fs :w<CR>
let g:lmap.f.s = 'Save buffer'

nnoremap <leader>fn :SyncNERDTree<CR>
let g:lmap.f.n = 'Tree sync'

let g:lmap.f.e = get(g:lmap.f, 'e', { 'name': 'DotF' })

nnoremap <leader>fed :e $MYVIMRC<CR>
let g:lmap.f.e.d = 'Open vimrc'

nnoremap <leader>fem :e $HOME/.config/nvim/modules/auto-modules.vim<CR>
let g:lmap.f.e.m = 'Open auto-modules.vim'
