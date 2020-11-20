" Move more sensibly when line wrapping enabled
nmap k gk
nmap j gj

" Switch windows with CTRL
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Move blocks of code
vnoremap < <gv
vnoremap > >gv

" Enable hlsearch on start
nnoremap / :set hlsearch<cr>/

" Clear search highlight
nnoremap <leader><leader> :noh<CR>
let g:lmap['\'] = 'Highlight off'

" Yank clipboard management
vnoremap <localleader>y "+y
nnoremap <localleader>y "+y
let g:llmap.y = 'Yank'
nnoremap <localleader>yy "+yg_
let g:llmap['yy'] = 'Yank to clipboard'

" Paste clipboard management
nnoremap <localleader>p "+p
vnoremap <localleader>p "+p
let g:llmap.p = 'Paste'
nnoremap <localleader>pp "+P
vnoremap <localleader>pp "+P
let g:llmap['pp'] = 'Paste from clipboard'

" Comment boxes
autocmd FileType vim vnoremap <buffer> ,cc !boxes -f ~/.config/nvim/cfg/boxes -d vim-box<CR>
autocmd FileType scala vnoremap <buffer> ,cc !boxes -f ~/.config/nvim/cfg/boxes -d scala<CR>
