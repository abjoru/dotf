let g:lmap.g = get(g:lmap, 'g', { 'name': 'Git' })

nnoremap <leader>gc :Gcommit<CR>
let g:lmap.g.c = 'Commit'

nnoremap <leader>gp :PushToCurrentBranch<CR>
let g:lmap.g.p = 'Push current branch'

nnoremap <leader>gs :Gstatus<CR>
let g:lmap.g.s = 'Status'

nnoremap <leader>gl :Commits!<CR>
let g:lmap.g.l = 'Log'

nnoremap <leader>gL :BCommits!<CR>
let g:lmap.g.L = 'Log current file'

nnoremap <leader>gd :Gdiff<CR>
let g:lmap.g.d = 'Git diff-tool'
