" This neovim configuration is heavily influenced by SpaceNeovim/SpaceVim

" Paths
let s:home_dir = $HOME
let s:config_dir = expand(resolve(s:home_dir . '/.config/nvim'))
let s:vim_plugged_dir = expand(resolve(s:config_dir . '/plugged'))
let s:modules_dir = expand(resolve(s:config_dir . '/modules'))
let s:cache_dir = expand(resolve(s:home_dir . '/.cache/nvim'))

" Files
let s:vim_plug_file = expand(resolve(s:config_dir . '/autoload/plug.vim'))
let s:loaded_plugins_cache_file = expand(resolve(s:cache_dir . '/loaded-plugins.vim'))
let s:bootstrap_lock_file = expand(resolve(s:config_dir . '/bootstrap.lock'))

" Modifiable state variables
let s:configuration_modules = get(g:, 'configuration_modules', [])
let s:additional_plugins = get(g:, 'additional_plugins', [])
let s:enabled_plugins = get(g:, 'enabled_plugins', [])

" Internal state variables
let s:is_install_bootstrapping = get(s:, 'is_install_bootstrapping', 0)
let s:install_output_array = get(s:, 'install_output_array', [])
let s:install_output_buff_num = get(s:, 'install_output_buff_num', -1)

" Globally accessible state
let g:dotf_enabled_modules = []

" Configurable variables
let g:dotf_debug = get(g:, 'dotf_debug', 0)
let g:dotf_verbose_debug = get(g:, 'dotf_verbose_debug', 0)
let g:dotf_module_debug = get(g:, 'dotf_module_debug', 0)

let g:dotf_leader_key = get(g:, 'dotf_leader_key', '<Space>')
let g:dotf_core_module = get(g:, 'dotf_core_module', 'core/base')

" vim-plug window location
let g:plug_window = 'vertical botright new'

"""""""
" API "
"""""""

function! dotf#init()
  " Defines a module to include
  " param1: name
  command! -nargs=1 -bar Module call s:module(<args>)
  " Defines a plugin to include
  " param1: name
  command! -nargs=+ -bar Plugin call dotf#plugin(<args>)

  " Configures theme
  " param1: background (i.e. 'dark')
  " param2: theme name (i.e. 'gruvbox')
  " param3: airline theme name (i.e. 'gruvbox')
  command! -nargs=+ -bar SetTheme call s:set_theme(<args>)

  " Enables debugging
  command! -nargs=0 -bar EnableDebug call s:enable_debug()
  " Enabled verbose debugging
  command! -nargs=0 -bar EnableVerboseDebug call s:enable_verbose_debug()
  " Enables module layer debugging
  command! -nargs=0 -bar EnableModuleDebug call s:enable_module_debug()

  " Returns the configured leader key
  command! -nargs=0 -bar GetLeader call dotf#get_leader_key()
  " Sets the leader key
  " param1: new leader key
  command! -nargs=1 -bar SetLeader call dotf#set_leader_key(<args>)

  " Invokes full upgrade/reload
  "command! -nargs=0 -bar DotfUpdate call dotf#update_all()
  " Make installer available to CLI
  command! -nargs=0 -bar DotfRunInstallProcess call dotf#install()

  call s:debug('>>> Initializing Dotf')
endfunction

"""""""""""""""""
" Bootstrapping "
"""""""""""""""""

function! dotf#bootstrap() abort
  if !exists('g:dotf_do_not_run_bootstrap')
    let l:called_from_bootstrap = 0
    call s:load_dotf_functionality(l:called_from_bootstrap)

    augroup dotf_plugin_update
      au!
      au VimEnter * call dotf#detect_changes_and_sync()
    augroup END
  endif
endfunction

function! s:load_dotf_functionality(called_from_bootstrap)
  let l:has_python = dotf#check_for_python()
  let l:modules = []
  let g:dotf_enabled_modules = get(g:, 'dotf_enabled_modules', {})

  call s:debug('>>> Parsing modules in ' . s:modules_dir . ':')
  let l:modules = dotf#find_all_modules(s:modules_dir)
  call s:debug('>>> Filtering enabled modules...')
  let g:dotf_enabled_modules = dotf#filter_enabled_modules(l:modules, s:configuration_modules)

  call s:verbose_debug('*** modules: ' . string(l:modules))
  call s:verbose_debug('*** enabled: ' . string(g:dotf_enabled_modules))

  " Source module util functions
  if filereadable(s:modules_dir . '/auto-modules.vim')
    execute 'source ' . s:modules_dir . '/auto-modules.vim'
  endif

  if l:has_python ==? 1 || exists('g:gui_oni')
    call dotf#install_enabled_plugins(g:dotf_enabled_modules, s:additional_plugins)
  endif

  call g:Dotf_postinstall()
endfunction

"""""""""""""""""""""
" Dotf Installation "
"""""""""""""""""""""

function! dotf#install() abort
  call s:setup_installation_state()

  " Check for python
  let l:has_python = dotf#check_for_python()
  if l:has_python ==? 0
    call s:output('IMPORTANT! Neovim could not find support for python, which means')
    call s:output('some modules may not work. To fix this, install the neovim python')
    call s:output('package. I.e. `pip install neovim` etc')
    call s:output('')
  endif

  call s:debug('>>> Starting Dotf bootstrap')

  if l:has_python ==? 1 || exists('g:gui_oni')
    call dotf#setup_vim_plug()
  endif

  let l:called_from_bootstrap = 1
  call s:load_dotf_functionality(l:called_from_bootstrap)
  call dotf#install_vim_plug_plugins()

  call s:debug('>>> Finished Dotf bootstrap')

  " Make sure we don't bootstrap again
  if writefile([], s:bootstrap_lock_file)
    call s:debug('>>> Writing bootstrap lock file')
  endif

  call s:debug('--- Installation finished, please restart Neovim! ---')
  ":quitall
endfunction

function! s:setup_installation_state()
  if s:is_install_bootstrapping == 0
    let s:is_install_bootstrapping = 1
    let s:install_output_array = []
    let s:install_output_buff_num = NewScratchBuffer()
    wincmd k " move back to original buffer (from new scratch buff)
  endif
endfunction

""""""""""""""""""""
" Module functions "
""""""""""""""""""""

function! s:module(module_name)
  call s:verbose_debug('--> User added module ' . a:module_name)

  if index(s:configuration_modules, a:module_name) ==? -1
    call add(s:configuration_modules, a:module_name)
  endif
endfunction

function! dotf#find_all_modules(dir) abort
  let l:located_modules = []
  for l:group in split(glob(a:dir . '/*'), '\n')
    for l:module in split(glob(l:group . '/*'), '\n')
      " make sure the module is not empty/invalid
      if filereadable(l:module . '/config.vim') || filereadable(l:module . '/packages.vim')
        let l:module_name = substitute(l:module, a:dir . '/', '', '')
        call add(l:located_modules, l:module_name)
        call s:verbose_debug('    Found ' . l:module_name)
      endif
    endfor
  endfor

  return l:located_modules
endfunction

function! dotf#filter_enabled_modules(located_modules, configured_modules) abort
  call s:verbose_debug('*** filter_enabled_modules(' . string(a:located_modules) . ', ' . string(a:configured_modules) . ')')
  let l:enabled_modules = []
  for l:c_module in a:configured_modules
    if index(a:located_modules, l:c_module) != -1
      call add(l:enabled_modules, l:c_module)
      call s:verbose_debug('    Enabled ' . l:c_module)
    endif
  endfor

  return l:enabled_modules
endfunction

function! dotf#load_module(module_name)
  if filereadable(s:modules_dir . '/' . a:module_name . '/func.vim')
    execute 'source ' . s:modules_dir . '/' . a:module_name . '/func.vim'
  endif

  if filereadable(s:modules_dir . '/' . a:module_name . '/packages.vim')
    execute 'source ' . s:modules_dir . '/' . a:module_name . '/packages.vim'
  endif

  if filereadable(s:modules_dir . '/' . a:module_name . '/config.vim')
    execute 'source ' . s:modules_dir . '/' . a:module_name . '/config.vim'
  endif
endfunction

""""""""""""""""""""
" Plugin functions "
""""""""""""""""""""

function! dotf#plugin(plugin_name, ...)
  call s:verbose_debug('--> User added extra plugin ' . a:plugin_name)
  let l:plugin_config = get(a:, '1', {})
  
  if index(s:additional_plugins, a:plugin_name) ==? -1
    call add(s:additional_plugins, {'name': a:plugin_name, 'config': l:plugin_config})
  endif
endfunction

function! dotf#install_enabled_plugins(enabled_modules, additional_plugins) abort
  call plug#begin(s:vim_plugged_dir)

  " explicitly load the core module first. The reason is because they might
  " be defined in other sources than the primary one.
  call s:debug('>>> Loading core module')
  if index(a:enabled_modules, g:dotf_core_module) != -1
    call dotf#load_module(g:dotf_core_module)
  endif

  call s:debug('>>> Loading remaining modules')
  for l:mname in a:enabled_modules
    if l:mname !=# g:dotf_core_module
      call dotf#load_module(l:mname)
    endif
  endfor

  call s:debug('>>> Installing plugins')
  for l:plugin in a:additional_plugins
    call s:verbose_debug('    ' . l:plugin.name)
    Plug l:plugin.name, l:plugin.config
    call add(s:enabled_plugins, l:plugin.name)
  endfor

  call plug#end()
endfunction

function! dotf#detect_plugin_changes()
  if !isdirectory(s:cache_dir)
    call mkdir(s:cache_dir, 'p')
  endif

  if filereadable(s:loaded_plugins_cache_file)
    let l:previously_loaded_plugins = readfile(s:loaded_plugins_cache_file)
  else
    let l:previously_loaded_plugins = []
  endif

  if l:previously_loaded_plugins == s:enabled_plugins
    call s:debug('>>> No changes in plugins')
  else
    call s:debug('>>> Plugins change detected, installing new ones')
    call dotf#install_vim_plug_plugins()
  endif
endfunction

function! dotf#detect_changes_and_sync()
  call dotf#detect_plugin_changes()
  call g:SyncConfiguration()
endfunction

function! dotf#write_plugins_to_cache()
  if !isdirectory(s:cache_dir)
    call mkdir(s:cache_dir, 'p')
  endif

  if writefile(s:enabled_plugins, s:loaded_plugins_cache_file)
    call s:debug('>>> Could not write loaded plugins to cache file!')
  endif
endfunction

"""""""""""
" VimPlug "
"""""""""""

function! dotf#setup_vim_plug() abort
  if empty(glob(s:vim_plug_file))
    call s:debug('>>> Downloading plug.vim')
    if has('nvim')
      let data = {'out': [], 'buf': s:install_output_buff_num}
      let l:job_opt = {
        \'on_stdout': function('OutputJobToBuffer', data),
        \'on_stderr': function('OutputJobToBuffer', data),
        \'on_exit': function('OutputJobToBuffer', data)
        \}
      let l:install_plug = jobstart([
        \ 'curl',
        \ '-fLo',
        \ s:vim_plug_file,
        \ '--create-dirs',
        \ 'https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
        \ ], l:job_opt)
      let l:await_job = jobwait([l:install_plug])
    else
      silent execute '!curl -fLo ' . s:vim_plug_file . ' --create-dirs https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
    endif

    call s:debug('>>> Sourcing ' . s:vim_plug_file)
    execute ":source " . s:vim_plug_file
  endif

  " if no plugins have been installed yet, make sure the cache file is empty!
  if !isdirectory(s:vim_plugged_dir)
    if writefile([], s:loaded_plugins_cache_file)
      call s:debug('>>> Overwriting cache file, since no plugins were installed!')
    endif
  endif
endfunction

function! dotf#install_vim_plug_plugins()
  call mkdir(s:vim_plugged_dir, 'p')
  call dotf#write_plugins_to_cache()
  call s:debug('>>> Installing all plugins via vim-plug')
  :PlugInstall! --sync
  call s:debug('>>> All plugins installed')
endfunction

"""""""""""""""""""""""
" API extra functions "
"""""""""""""""""""""""

function! dotf#get_leader_key()
  return g:dotf_leader_key
endfunction

function! dotf#set_leader_key(new_leader)
  let g:dotf_leader_key = a:new_leader
endfunction

function! s:set_theme(bg, tname, ...)
  if a:bg ==? 'light'
    set background=light
  else
    set background=dark
  endif

  try
    if (has('termguicolors'))
      set termguicolors
    endif

    execute 'colorscheme ' . a:tname
    hi Comment cterm=italic

    if a:0 ==? 1
      let g:airline_theme = a:1
    endif
  catch
  endtry
endfunction

function! s:enable_debug()
  let g:dotf_debug = 1
endfunction

function! s:enable_verbose_debug()
  let g:dotf_verbose_debug = 1
endfunction

function! s:enable_module_debug()
  let g:dotf_module_debug = 1
endfunction

""""""""""""""""""
" Util functions "
""""""""""""""""""

function! s:output(msg)
  if !has('nvim') || s:install_output_buff_num == -1
    echo a:msg
  else
    let s:install_output_array += [a:msg]
    call OutputListToBuffer(s:install_output_buff_num, s:install_output_array)
  endif
endfunction

function! s:debug(msg)
  if g:dotf_debug || s:is_install_bootstrapping
    if !has('nvim') || s:install_output_buff_num == -1
      echo a:msg
    else
      let s:install_output_array += [a:msg]
      call OutputListToBuffer(s:install_output_buff_num, s:install_output_array)
    endif
  endif
endfunction

function! s:verbose_debug(msg)
  if g:dotf_verbose_debug 
    if !has('nvim') || s:install_output_buff_num == -1
      echo a:msg
    else
      let s:install_output_array += [a:msg]
      call OutputListToBuffer(s:install_output_buff_num, s:install_output_array)
    endif
  endif
endfunction

function! dotf#check_for_python()
  return has('python') || has('python3')
endfunction

""""""""""""""""""""
" Helper functions "
""""""""""""""""""""
" Avoid vint complaining about self
" vint: -ProhibitImplicitScopeVariable -ProhibitUsingUndeclaredVariable 

function! NewScratchBuffer()
  rightbelow new
  setlocal nobuflisted buftype=nofile bufhidden=wipe noswapfile
  return bufnr('%')
endfunction

" Make sure we don't redefine these functions on re-sourcing
if !exists('g:dotf_buffer_output_functions_defined')
  let g:dotf_buffer_output_functions_defined = 1

  function! OutputListToBuffer(bnr, out)
    call nvim_buf_set_lines(a:bnr, 0, -1, v:true, a:out)
    redraw
  endfunction

  function! OutputJobToBuffer(jid, data, event) dict
    if a:event ==? 'stdout' || a:event ==? 'stderr'
      let self.out += a:data
      call nvim_buf_set_lines(self.buf, 0, -1, v:true, self.out)
    endif
  endfunction

endif

" Make sure we don't redefine these functions on re-sourcing
if !exists('g:dotf_update_and_sync_already_defined')
  let g:dotf_update_and_sync_already_defined = 1

  function! g:SyncConfiguration(...)
    if a:0 > 0
      let l:buf_nr = a:1
      let l:out = a:2
      let l:out += ["Syncing configuration, please hold on!..."]
      call OutputListToBuffer(l:buf_nr, l:out)
      echo "Wait for the sync to finish!"

      let l:out += ["    Setting dotf_postinit_loaded to 0", "    Sourcing $MYVIMRC"]
      call OutputListToBuffer(l:buf_nr, l:out)

      " start the sync
      let g:dotf_postinit_loaded = 0
      :source $MYVIMRC

      let l:out += ["    Calling post initialization"]
      call OutputListToBuffer(l:buf_nr, l:out)
      call g:Dotf_postinit()

      let l:out += ["Finished configuration sync!"]
      call OutputListToBuffer(l:buf_nr, l:out)
      echo "You are good to go!"
    else
      let g:dotf_postinit_loaded = 0
      :source $MYVIMRC
      call g:Dotf_postinit()
    endif
  endfunction
endif

" vint: +ProhibitImplicitScopeVariable +ProhibitUsingUndeclaredVariable 
