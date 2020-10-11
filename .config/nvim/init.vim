" ~/.vimrc

set nocompatible

" Configuration for vim-plug
call plug#begin('~/.cache/nvim/plugged')
Plug 'neoclide/coc.nvim', {'branch': 'release'}
Plug 'scrooloose/nerdtree'
Plug 'preservim/nerdcommenter'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'tiagofumo/vim-nerdtree-syntax-highlight'
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'
Plug 'ctrlpvim/ctrlp.vim'
Plug 'samoshkin/vim-mergetool'
Plug 'mariappan/dragvisuals.vim'
Plug 'morhetz/gruvbox'
Plug 'ryanoasis/vim-devicons'
Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
Plug 'vimwiki/vimwiki'
Plug 'chrisbra/colorizer'
call plug#end()

" General
set shiftwidth=2
set autoindent
set autoread
set smartindent
set encoding=UTF-8
set expandtab
set number relativenumber 	" Display line numbers
set t_Co=256 			" Set if term supports 256 colors.
set noshowmode 			" Mode is shown in status line, so no need for another

autocmd TermOpen * setlocal nonumber

colorscheme gruvbox
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

" Path additions
set path+=**

" Window management
set splitbelow
set splitright

" Column Limits
highlight ColorColumn ctermbg=Black
set colorcolumn=121

" Configuration for vim-scala
au BufRead,BufNewFile *.sbt set filetype=scala

autocmd FileType json syntax match Comment +\/\/.\+$+

" Plugin Configuration
source ~/.config/nvim/nerdtree.vim
source ~/.config/nvim/airline.vim
source ~/.config/nvim/coc-metals.vim
source ~/.config/nvim/mergetool.vim
source ~/.config/nvim/dvisuals.vim
source ~/.config/nvim/vim-notes.vim
source ~/.config/nvim/colorizer.vim

" Better window resizing
map <silent> <A-h> <C-w><
map <silent> <A-j> <C-w>-
map <silent> <A-k> <C-w>+
map <silent> <A-l> <C-w>>

" Clipboard management
vnoremap  <leader>y  "+y
nnoremap  <leader>Y  "+yg_
nnoremap  <leader>y  "+y

nnoremap <leader>p "+p
nnoremap <leader>P "+P
vnoremap <leader>p "+p
vnoremap <leader>P "+P

" Fuzzy search
set wildmenu
set wildignore+=*/tmp/*,*.so,*.swp,*.zip,*.class,*.hi,*.o,*/target/*,*/target,*/out

" --column: Show column number
" --line-number: Show line number
" --no-heading: Do not show file headings in results
" --fixed-strings: Search term as a literal string
" --ignore-case: Case insensitive search
" --no-ignore: Do not respect .gitignore, etc...
" --hidden: Search hidden files and folders
" --follow: Follow symlinks
" --glob: Additional conditions for search (in this case ignore everything in the .git/ folder)
" --color: Search color options
command! -bang -nargs=* Find 
  \ call fzf#run(fzf#wrap({'source': 'rg --files --hidden --glob "!{.git/*}"', 'down': '40%', 'options': '--expect=ctrl-t,ctrl-x,ctrl-v --multi --reverse'}))

nnoremap <silent> <leader>o :Find<CR>

command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   'rg --column --line-number --no-heading --color=always --smart-case '.shellescape(<q-args>), 1, 
  \   fzf#vim#with_preview(), <bang>0)
