"let g:dfCocHoverInfo = get(g:, 'dfCocHoverInfo', 1)

"let g:coc_global_extensions = get(g:, 'coc_global_extensions', [
      "\'coc-rust-analyzer',
      "\'coc-go',
      "\'coc-python',
      "\'coc-flutter',
      "\'coc-java',
      "\'coc-phpls',
      "\'coc-fsharp',
      "\'coc-solargraph',
      "\'coc-metals',
      "\'coc-floaterm',
      "\'coc-sourcekit',
      "\'coc-vimlsp',
      "\'coc-xml',
      "\'coc-tsserver',
      "\'coc-json',
      "\'coc-vetur',
      "\'coc-markdownlint',
      "\'coc-html',
      "\'coc-eslint',
      "\'coc-css',
      "\'coc-prettier',
      "\'coc-highlight',
      "\'coc-explorer',
      "\'coc-cmake',
      "\'coc-yank',
      "\'coc-snippets',
      "\'coc-git'
      "\])
let g:coc_global_extensions = get(g:, 'coc_global_extensions', [
      \'coc-metals',
      \'coc-vimlsp',
      \'coc-json',
      \'coc-prettier',
      \'coc-highlight',
      \'coc-git'
      \])

DfLMap 'm', 'Metals'
DfLLMap 'q', 'Quick'

" TextEdit might fail if hidden is not set
set hidden

" Some servers have issues with backup files, see #649
set nobackup
set nowritebackup

" Give more space for displaying messages.
set cmdheight=2

" Having longer updatetime (default is 4000 ms = 4s) leads to noticeable
" delays and poor user experience.
set updatetime=300

" Don't pass messages to |ins-completion-menu|.
set shortmess+=c

" Always show the signcolumn, otherwise it would shift the text each time
" diagnostics appear/become resolved.
if has("patch-8.1.1564")
  "Recently vim can merge signcolumn and number column into one
  set signcolumn=number
else
  set signcolumn=yes
endif

" Use <tab> for trigger completion with characters ahead and navigate.
" NOTE: Use command `:verbose imap <tab>` to make sure tab is not mapped by
" other plugins before putting this into your config.
inoremap <silent><expr> <TAB>
      \ pumvisible() ? "\<C-n>" :
      \ <SID>check_back_space() ? "\<TAB>" :
      \ coc#refresh()
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
nmap <localleader>rn <Plug>(coc-rename)
let g:llmap['rn'] = 'Rename word'

" remap for format selected regions
xmap <localleader>f <Plug>(coc-format-selected)
nmap <localleader>f <Plug>(coc-format-selected)
let g:llmap.f = 'Format selected'

augroup mygroup
  autocmd!
  " setup formatexpr specified filetype(s)
  autocmd FileType scala setl formatexpr=CocAction('formatSelected')
  " update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup END

" remap for do codeAction of selected region, ex: `<leader>aap` for current
" paragraph.
xmap <localleader>a <Plug>(coc-codeaction-selected)
nmap <localleader>a <Plug>(coc-codeaction-selected)
let g:llmap.a = 'Code action for selected'

" remap for do codeAction of current line
nmap <localleader>ac <Plug>(coc-codeaction)
let g:llmap['ac'] = 'Code action for current line'

" fix autofix problem of current line
nmap <localleader>qf <Plug>(coc-fix-current)
let g:llmap.q.f = 'QuickFix current'

" use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" use `:Fold` to fold current buffer
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" add status line support, for integration with other plugin, checkout `:h
" coc-status`
set statusline^=%{coc$status()}%{get(b:,'coc_current_function','')}

" show all diagnostics
DfNMap 'ma', 'list-diagnostics', '<C-u>CocList diagnostics'
" manage extensions
DfNMap 'me', 'list-extensions', '<C-u>CocList extensions'
" show commands
DfNMap 'mc', 'list-commands', '<C-u>CocList commands'
" find symbol of current document
"nnoremap <silent> <space>o :<C-u>CocList outline<cr>
DfNMap 'mo', 'list-outline', '<C-u>CocList outline'
" search workspace symbols
"nnoremap <silent> <space>s :<C-u>CocList -I symbols<cr>
DfNMap 'ms', 'list-symbols', '<C-u>CocList -I symbols'
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
