let g:lmap.f = get(g:lmap, 'f', { 'name': 'Files' })

nnoremap <leader>ff :Files<CR>
let g:lmap.f.f = 'Find file'

nnoremap <leader>fb :Buffers<CR>
let g:lmap.f.b = 'Find buffer'

nnoremap <leader>fl :Lines<CR>
let g:lmap.f.l = 'Find line'

if executable('rg')
  nnoremap <leader>fg :Rg<CR>
  let g:lmap.f.g = 'RipGrep'
endif

if DotF#modules#is_enabled('scm/git')
  nnoremap <leader>fG :GGrep<CR>
  let g:lmap.f.G = 'GitGrep'
endif
