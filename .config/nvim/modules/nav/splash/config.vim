let s:LIST = DotF#api#import('list')

" Don't change to directory of file opened from startify
let g:startify_change_to_dir = 0

" Generate startify lists with task support
let g:startify_lists = g:Dotf_build_startify_lists()

augroup fixStartify
  au!
  au User Startified nmap <buffer> k k
  au User Startified nmap <buffer> j j
  au User Startified nmap <buffer> q :q<CR>
augroup END
