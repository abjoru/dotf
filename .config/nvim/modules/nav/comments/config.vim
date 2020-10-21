" remove default mappings
let g:NERDCreateDefaultMappings = 1

let g:lmap.c = {'name': 'Comments'}

let g:lmap.c.l = 'comment-or-uncomment-lines'
nmap <leader>cl gcc
vmap <leader>cl gc

let g:lmap.c.y = 'copy-and-comment-lines'
nmap <leader>cy yygcc
vmap <leader>cy ygvgc

DfMap 'cc', 'comment-or-uncomment-lines-inverted', 'DfInvertToggleComment'

" add spaces after comment delimiters by default
let g:NERDSpaceDelims = 1

" allow commenting and inverting empty lines
let g:NERDCommentEmptyLines = 1

" enable trimming of trailing whitespace when uncommenting
let g:NERDTrimTrailingWhitespace = 1

" enable toggle to check all selected lines if commented or not
let g:NERDToggleCheckAllLines = 1

" use compact syntax for prettified multiline comments
let g:NERDCompactSexyComs = 1
