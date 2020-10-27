let s:LOG = DotF#logger#derive('plug')
let s:UTILS = DotF#api#import('utils')
let s:CACHE = DotF#api#import('cache')

" Directories and files
let s:plug_dir = expand(resolve(g:config_dir . '/plugged'))
let s:plug_file = expand(resolve(g:config_dir . '/autoload/plug.vim'))
let s:cache_file = expand(resolve(g:cache_dir . '/loaded-plugins.vim'))

" Download and install vim-plug
function! DotF#plug#download() abort
  if empty(glob(s:plug_file))
    call s:LOG.info('Downloading plug.vim...')
    if has('nvim')
      let data = {'out': [], 'buf': g:scratch_buffer_no}
      let l:job_opt = {
        \ 'on_stdout': function('s:append_to_buffer', data),
        \ 'on_stderr': function('s:append_to_buffer', data),
        \ 'on_exit': function('s:append_to_buffer', data)
        \ }
      let l:install_job = jobstart([
        \ 'curl',
        \ '-fLo',
        \ s:plug_file,
        \ '--create-dirs',
        \ 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
        \ ], l:job_opt)
      let l:await_job = jobwait([l:install_job])
    else
      silent execute '!curl -fLo ' . s:plug_file . ' --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim' 
    endif

    call s:LOG.info('Sourcing ' . s:plug_file)
    call s:UTILS.source(s:plug_file)
  endif

  if !isdirectory(s:plug_dir)
    if writefile([], s:cache_file)
      call s:LOG.warn('Overwriting cache file, since no plugins were installed!')
    endif
  endif
endfunction

" Install/sync all plugins using vim-plug
function! DotF#plug#install() abort
  call mkdir(s:plug_dir, 'p')
  call s:CACHE.write_plugins()
  call s:LOG.info('Installing all plugins via vim-plug')
  :PlugInstall! --sync
  call s:LOG.info('All plugins installed')
endfunction
