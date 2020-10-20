set nocompatible
set encoding=UTF-8

" shorten time before the vim-leader-guide buffer appears
set timeoutlen=300
set ttimeoutlen=0

" use system clipboard
set clipboard=unnamed
" disable swap
set noswapfile
" read modelines from files
set modeline

" display menu keys with a "+" if expandable
let g:leaderGuide_display_plus_menus = 1

" FIXME don't show line numbers in terminal
autocmd TermOpen * setlocal nonumber

"""""""""""""""""""""""""""""
" Whitespace, tabs & spaces "
"""""""""""""""""""""""""""""

set shiftwidth=2      	" number of spaces pr tab when indenting
set autoindent
set autoread
set smartindent
set list                " show invisible characters
set expandtab           " indent with spaces
set softtabstop=2     	" number of spaces pr tab when inserting
set tabstop=2         	" number of spaces tab counts for

""""""
" UI "
""""""

let $NVIM_TUI_ENABLE_TRUE_COLOR=1
set laststatus=2
set scrolloff=7       	" set 7 lines to the cursor - when moving vertically using j/k
set splitbelow          " split below
set splitright          " split on the right side
set number              " show current line number
set relativenumber      " enable relative line numbers
set noshowmode          " mode is shown in status line, no need for another one

""""""""
" Font "
""""""""

"set guifont=Droid\ Sans\ Mono\ for\ Powerline\ Nerd\ Font\ Complete\ 12

"""""""""""
" Backups "
"""""""""""

set nobackup
set nowritebackup
set undofile
set undolevels=1000   	" max num of changes that can be undone
set undoreload=10000  	" max num lines to save for undo on a buffer reload

""""""""""
" Search "
""""""""""

set hlsearch            " highlight matches
set incsearch           " search as chars are entered
set ignorecase          " ignore case in searches
set smartcase           " unless casing in query

""""""""""""
" Mappings "
""""""""""""

" move more sensibly when line wrapping enabled
nmap k gk
nmap j gj

" switch windows with CTRL
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" move blocks of code
vnoremap < <gv
vnoremap > >gv

" enable hlsearch on start
nnoremap / :set hlsearch<cr>/
" clear search highlight on 'spc spc'
nnoremap <leader><leader> :noh<CR>
let g:llmap['/'] = [':noh', 'Clear highlights']

" Clipboard management
vnoremap <localleader>y "+y
nnoremap <localleader>yy "+yg_
nnoremap <localleader>y "+y

nnoremap <localleader>p "+p
nnoremap <localleader>pp "+P
vnoremap <localleader>p "+p
vnoremap <localleader>pp "+P

"""""""""""""""""
" Uncategorized "
"""""""""""""""""

" make super tab start from the top and go down
let g:SuperTabDefaultCompletionType = "<c-n>"

" enable mouse mode
set mouse=a

" make enter select item when completion menu is visible
inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" set messages to be short
set shortmess=at

" set sotf wrap indent to be ` .`.
set breakindent
set breakindentopt=shift:0
set showbreak=\ \·

set wildmenu
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.class,*.hi,*.o,*/target/*,*/target,*/out,*/.bloop
