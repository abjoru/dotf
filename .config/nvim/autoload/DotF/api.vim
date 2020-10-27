"""""""""""""""""""""""""
" Based on SpaceVim API "
"""""""""""""""""""""""""

" To use the API, make sure DotF has been added to your &rtp and then use the
" `DotF#api#import` to import the API you need.
"
" Example:
" let s:log = DotF#api#import('logger')
" let rst = s:log.info('msg')
" let rst = s:log.warn('msg')

" the api itself is a dict
let s:apis = {}

""
" Import API base the given `name`, and return the API object. 
function! DotF#api#import(name) abort
  if has_key(s:apis, a:name)
    return deepcopy(s:apis[a:name])
  endif

  let p = {}
  try 
    let p = DotF#api#{a:name}#get()
    let s:apis[a:name] = deepcopy(p)
  catch /^Vim\%((\a\+)\)\=:E117/
  endtry

  return p
endfunction

""
" Register some API
function! DotF#api#register(name, api) abort
  if !empty(DotF#api#import(a:name))
    echoerr '[DotF API] Api: ' . a:name . ' already exists!'
  else 
    let s:apis[a:name] = deepcopy(a:api)
  endif
endfunction
