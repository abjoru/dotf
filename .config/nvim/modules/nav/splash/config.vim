let g:startify_custom_header = get(g:, 'startify_custom_header', [
\'',
\'          [D o t F - N e o V i m - B u i l d]',
\'',
\])

augroup fixStartify
  au!
  au User Startified nmap <buffer> k k
  au User Startified nmap <buffer> j j
  au User Startified nmap <buffer> q :q<CR>
augroup END
