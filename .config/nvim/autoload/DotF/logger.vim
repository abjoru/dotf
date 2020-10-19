let s:LOGGER = DotF#api#import('logger')

call s:LOGGER.set_name('DotF')
call s:LOGGER.set_level(1)
call s:LOGGER.set_silent(1)
call s:LOGGER.set_verbose(1)

function! DotF#logger#info(msg) abort
  call s:LOGGER.info(msg)
endfunction

function! DotF#logger#warn(msg, ...) abort
  let issilent = get(a:000, 0, 1)
  call s:LOGGER.warn(a:msg, issilent)
endfunction

function! DotF#logger#error(msg) abort
  call s:LOGGER.error(msg)
endfunction

function! DotF#logger#debug(msg) abort
  call s:LOGGER.debug(msg)
endfunction

function! DotF#logger#viewRuntimeLog() abort
  let info = "### DotF runtime log :\n\n"
  let info .= "```log\n"

  let info .= s:LOGGER.view(s:LOGGER.level)

  let info .= "\n```\n"
  tabnew +setl\ nobuflisted
  nnoremap <buffer><silent> q :bd!<CR>
  for msg in split(info, "\n")
    call append(line('$'), msg)
  endfor
  normal! "_dd
  setl nomodifiable
  setl buftype=nofile
  setl filetype=markdown
  call s:syntax_extra()
endfunction

function! s:syntax_extra() abort
  call matchadd('ErrorMsg', '.*[\sError\s\].*')
  call matchadd('WarningMsg', '.*[\sWarn\s\].*')
endfunction

function! DotF#logger#setlevel(level) abort
  call s:LOGGER.set_level(a:level)
endfunction

function! DotF#logger#setoutput(file) abort
  call s:LOGGER.set_file(a:file)
endfunction

let s:derive = {}
let s:derive.origin_name = s:LOGGER.get_name()

function! s:derive.info(msg) abort
  call s:LOGGER.set_name(self.derive_name)
  call s:LOGGER.info(a:msg)
  call s:LOGGER.set_name(self.origin_name)
endfunction

function! s:derive.warn(msg) abort
  call s:LOGGER.set_name(self.derive_name)
  call s:LOGGER.warn(a:msg)
  call s:LOGGER.set_name(self.origin_name)
endfunction

function! s:derive_error(msg) abort
  call s:LOGGER.set_name(self.derive_name)
  call s:LOGGER.error(a:msg)
  call s:LOGGER.set_name(self.origin_name)
endfunction

function! s:derive_debug(msg) abort
  call s:LOGGER.set_name(self.derive_name)
  call s:LOGGER.debug(a:msg)
  call s:LOGGER.set_name(self.origin_name)
endfunction

function! DotF#logger#derive(name) abort
  let s:derive.derive_name = printf('%' . strdisplaywidth(s:LOGGER.get_name()) . 'S', a:name)
  return deepcopy(s:derive)
endfunction
