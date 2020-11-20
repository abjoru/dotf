"""""""""""""""""""
" Top Level Menus "
"""""""""""""""""""

DfLMap 'm', 'Metals'
DfLLMap 'q', 'Quick'

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

" remap for rename current word
nmap <localleader>rn <Plug>(coc-rename)
let g:llmap['rn'] = 'Rename word'

" remap for format selected regions
xmap <localleader>f <Plug>(coc-format-selected)
nmap <localleader>f <Plug>(coc-format-selected)
let g:llmap.f = 'Format selected'

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

" show all diagnostics
DfNMap 'ma', 'List diagnostics', '<C-u>CocList diagnostics'
" manage extensions
DfNMap 'me', 'List extensions', '<C-u>CocList extensions'
" show commands
DfNMap 'mc', 'List commands', '<C-u>CocList commands'
" find symbol of current document
DfNMap 'mo', 'List outline', '<C-u>CocList outline'
" search workspace symbols
DfNMap 'ms', 'List symbols', '<C-u>CocList -I symbols'
" do default action for next item
DfNMap 'j', 'Next', '<C-u>CocNext'
" do default action for previous item.
DfNMap 'k', 'Previous', '<C-u>CocPrev'
" resume latest coc list
DfNMap 'p', 'Resume list', '<C-u>CocListResume'

" notify coc.nvim that <enter> has been pressed.
" currently used for the formatOnType feature
inoremap <silent><expr> <cr> pumvisible() ? coc#_select_confirm()
      \: "\<C-g>u\<CR>\<c-r>=coc#on_enter()\<CR>"
