let s:LOG = DotF#logger#derive('automodule')

" Paths
let s:config_dir = $HOME . '/.config/nvim'
let s:modules_dir = expand(resolve(s:config_dir . '/modules'))

" Halt if dotf already loaded
if exists('g:loaded_dotf')
  finish
endif
let g:loaded_dotf = 1

" Setup default plugin configuration
function! s:dotf_preinstall()
  call s:LOG.warn('Run pre-install')

  " create default vim-leader-guide map, etc
  let g:lmap = get(g:, 'lmap', {})

  " create default localleader map
  let g:llmap = get(g:, 'llmap', {})

  " reset nerdcommenter key mappings
  let g:NERDCreateDefaultMappings = get(g:, 'NERDCreateDefaultMappings', 0)

  " reset vim-easymotion key mappings
  let g:EasyMotion_do_mapping = get(g:, 'EasyMotion_do_mapping', 0)

  " reset vim-gitgutter key mappings
  let g:gitgutter_map_keys = get(g:, 'gitgutter_map_keys', 0)

  " reset vim-swoop key mappings
  let g:swoopUseDefaultKeyMap = get(g:, 'swoopUseDefaultKeyMap', 0)
endfunction

function! g:Dotf_postinit()
  call s:LOG.warn('Run postinit')

  " configure vim-leader-guide
  if exists('g:loaded_leaderGuide_vim')
    call s:LOG.warn('Configure vim-leader-guide')

    " Match all keys in a sequence
    "let g:leaderGuide_match_whole = 1

    " cleanup displayed key bindings
    function! s:dotf_displayfunc()
      "let g:leaderGuide#displayname = substitute(g:leaderGuide#displayname, '\c<cr>$', '', '')
      let g:leaderGuide#displayname = substitute(g:leaderGuide#displayname, '^<SID>', '', '')
      let g:leaderGuide#displayname = substitute(g:leaderGuide#displayname, '^<Plug>', '', '')
      let g:leaderGuide#displayname = substitute(g:leaderGuide#displayname, '#', '', '')
    endfunction

    " add custom display func if found
    if exists('g:leaderGuide_displayfunc')
      call add(g:leaderGuide_displayfunc, function('s:dotf_displayfunc'))
    else
      let g:leaderGuide_displayfunc = [function('s:dotf_displayfunc')]
    endif

    " map the leader key to <Space> (or defined one)
    let l:leader_key = dotf#get_leader_key()
    if l:leader_key ==? '<Space>'
      let g:mapleader = ' '
    else
      let g:mapleader = l:leader_key
    endif
  
    " register localleader map
    call leaderGuide#register_prefix_descriptions('\', 'g:llmap')

    " create toplevel dict
    let g:topdict = {}
    let g:topdict[' '] = g:lmap
    let g:topdict[' ']['name'] = '<leader>'
    let g:topdict['\'] = g:llmap
    let g:topdict['\']['name'] = '<localleader>'
    call leaderGuide#register_prefix_descriptions("", 'g:topdict')

    execute 'nnoremap <silent> <Leader> :<c-u>LeaderGuide "' . l:leader_key . '"<CR>'
    execute 'vnoremap <silent> <Leader> :<c-u>LeaderGuideVisual "' . l:leader_key . '"<CR>'
    nnoremap <silent> <localleader> :<c-u>LeaderGuide '\'<CR>
    nnoremap <silent> ? :<c-u>LeaderGuide ""<CR>
  endif

  if exists('g:loaded_webdevicons')
    call webdevicons#refresh()
  endif
endfunction

function! g:Dotf_postinstall()
  try
    call s:LOG.debug('Run post install')
    let l:leader_key = dotf#get_leader_key
    call leaderGuide#register_prefix_descriptions(l:leader_key, 'g:lmap')
  catch
  endtry
endfunction

call s:dotf_preinstall()

if !exists('g:dotf_postinit_loaded')
  let g:dotf_postinit_loaded = 1

  augroup dotf_postinit
    autocmd!
    autocmd VimEnter * call g:Dotf_postinit()
  augroup END
endif
