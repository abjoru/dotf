let g:coc_global_extensions = get(g:, 'coc_global_extensions', [
      \'coc-metals',
      \'coc-vimlsp',
      \'coc-json',
      \'coc-prettier',
      \'coc-highlight',
      \'coc-git'
      \])

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

" hightlight symbol under cursor on cursorhold
autocmd CursorHold * silent call CocActionAsync('highlight')

augroup mygroup
  autocmd!
  " setup formatexpr specified filetype(s)
  autocmd FileType scala setl formatexpr=CocAction('formatSelected')
  " update signature help on jump placeholder
  autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
augroup END

" use `:Format` to format current buffer
command! -nargs=0 Format :call CocAction('format')

" use `:Fold` to fold current buffer
command! -nargs=? Fold :call CocAction('fold', <f-args>)

" add status line support, for integration with other plugin, checkout `:h
" coc-status`
set statusline^=%{coc$status()}%{get(b:,'coc_current_function','')}
