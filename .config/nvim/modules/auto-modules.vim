function! s:debug(msg)
  if g:dotf_module_debug
    echom a:msg
  endif
endfunction

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
  call s:debug('>>> Run pre-install')

  " create default vim-leader-guide map, etc
  let g:lmap = get(g:, 'lmap', {})
  let g:lmap.m = get(g:lmap, 'm', {'name': 'major-mode-cmd'})

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
  call s:debug('>>> Run postinit')

  " configure vim-arpeggio
  if exists('g:loaded_arpeggio')
    if exists('g:dotf_escape_key_sequence')
      call arpeggio#map('i', '', 0, g:dotf_escape_key_sequence, '<Esc>')
    endif
    let g:arpeggio_timeoutlen = get(g:, 'arpeggio_timeoutlen', 100)
  endif

  " configure vim-leader-guide
  if exists('g:loaded_leaderGuide_vim')
    call s:debug('>>> Configure vim-leader-guide')

    " cleanup displayed key bindings
    function! s:dotf_displayfunc()
      let g:leaderGuide#displayname = substitute(g:leaderGuide#displayname, '\c<cr>$', '', '')
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
    execute 'nnoremap <silent> <Leader> :<c-u>LeaderGuide "' . l:leader_key . '"<CR>'
    execute 'vnoremap <silent> <Leader> :<c-u>LeaderGuideVisual "' . l:leader_key . '"<CR>'
  endif
endfunction

function! g:Dotf_postinstall()
  try
    call s:debug('>>> Run post install')
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
