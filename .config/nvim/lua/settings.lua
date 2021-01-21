local M = {}
local g = vim.g
local cmd = vim.cmd
local api = require('api')

local indent = 2

M.setup = function()
  cmd('set nocompatible')
  cmd('set encoding=UTF-8')

  -- shorten time before the vim-leader-guide buffer appears
  cmd('set timeoutlen=300')
  cmd('set ttimeoutlen=0')

  -- use system clipboard
  cmd('set clipboard=unnamed')
  -- disable swap
  cmd('set noswapfile')
  -- read modelines from files
  cmd('set modeline')

  -- don't show line numbers in terminals
  cmd [[autocmd TermOpen * setlocal nonumber]]

  -- Leaders
  g['mapleader'] = ' '
  g['maplocalleader'] = '/'


end

return M
