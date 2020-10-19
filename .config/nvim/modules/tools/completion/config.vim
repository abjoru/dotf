let g:dfCocHoverInfo = get(g:, 'dfCocHoverInfo', 1)

let g:coc_global_extensions = get(g:, 'coc_global_extensions', [
      \'coc-rust-analyzer',
      \'coc-go',
      \'coc-python',
      \'coc-flutter',
      \'coc-java',
      \'coc-phpls',
      \'coc-fsharp',
      \'coc-solargraph',
      \'coc-metals',
      \'coc-floaterm',
      \'coc-sourcekit',
      \'coc-vimlsp',
      \'coc-xml',
      \'coc-tsserver',
      \'coc-json',
      \'coc-vetur',
      \'coc-markdownlint',
      \'coc-html',
      \'coc-eslint',
      \'coc-css',
      \'coc-prettier',
      \'coc-highlight',
      \'coc-explorer',
      \'coc-cmake',
      \'coc-yank',
      \'coc-snippets',
      \'coc-git'
      \])

set hidden
set nobackup
set nowritebackup
set cmdheight=2
set updatetime=300
set shortmess+=c
set signcolumn=yes

" use tab for trigger completion with characters ahead and navigate. 
inoremap <silent><expr> <TAB>
      \pumvisible() ? "\<C-n>" :
      \<SID>check_back_space() ? "\<TAB>" :
      \coc#refresh()
inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"

function! s:check_back_space() abort
  let col = col('.') - 1
  return !col || getline('.')[col - 1] =~# '\s'
endfunction

" use <c-space> to trigger completion
inoremap <silent><expr> <c-space> coc#refresh()

" use <cr> to confirm completion, `<C-g>u` means break undo chain
" at current position. Coc only does snippets and additional edit on confirm.
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"

" use `[c` and `]c` to navigate diagnostics
nmap <silent> [c <Plug>(coc-diagnostic-prev)
nmap <silent> ]c <Plug>(coc-diagnostic-next)

" remap keys for gotos
nmap <silent> gd <Plug>(coc-definition)
nmap <silent> gy <Plug>(coc-type-definition)
nmap <silent> gi <Plug>(coc-implementation)
nmap <silent> gr <Plug>(coc-references)

" used to expand decorations in worksheets (impl in func.vim)
nmap <silent> K :call <SID>show_documentation()<CR>

" hightlight symbol under cursor on cursorhold
autocmd CursorHold * silent call CocActionAsync('highlight')

" remap for rename current word
nmap <leader>rn <Plug>(coc-rename)

" remap for format selected regions
xmap <leader>f <Plug>(coc-format-selected)
nmap <leader>f <Plug>(coc-format-selected)

augroup mygroup
  autocmd!
  " setup formatexpr specified filetype(s)
  autocmd FileType scala setl formatexpr=CocAction('formatSelected')
  " update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup END

" remap for do codeAction of selected region, ex: `<leader>aap` for current
" paragraph.
xmap <leader>a <Plug>(coc-codeaction-selected)
nmap <leader>a <Plug>(coc-codeaction-selected)

" remap for do codeAction of current line
nmap <leader>ac <Plug>(coc-codeaction)
" fix autofix problem of current line
nmap <leader>qf <Plug>(coc-fix-current)

" use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" use `:Fold` to fold current buffer
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" add status line support, for integration with other plugin, checkout `:h
" coc-status`
set statusline^=%{coc$status()}%{get(b:,'coc_current_function','')}

" show all diagnostics
"nnoremap <silent> <space>a :<C-u>CocList diagnostics<cr>
" manage extensions
"nnoremap <silent> <space>e :<C-u>CocList extensions<cr>
" show commands
"nnoremap <silent> <space>c :<C-u>CocList commands<cr>
" find symbol of current document
"nnoremap <silent> <space>o :<C-u>CocList outline<cr>
" search workspace symbols
"nnoremap <silent> <space>s :<C-u>CocList -I symbols<cr>
" do default action for next item
"nnoremap <silent> <space>j :<C-u>CocNext<CR>
" do default action for previous item.
"nnoremap <silent> <space>k :<C-u>CocPrev<CR>
" resume latest coc list
"nnoremap <silent> <space>p :<C-u>CocListResume<CR>

" notify coc.nvim that <enter> has been pressed.
" currently used for the formatOnType feature
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
      \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"