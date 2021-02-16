local M = {}
local g = vim.g
local cmd = vim.cmd
local utils = require('dotf/utils')

local indent = 2

local function tabsAndSpacesSettings()
  --cmd('set autoindent')
  --cmd('set autoread')
  --cmd('set smartindent')
  --cmd('set list')
  utils.opt('b', 'expandtab', true)
  utils.opt('b', 'shiftwidth', indent)
  utils.opt('b', 'tabstop', indent)
  utils.opt('b', 'softtabstop', indent)
  utils.opt('b', 'fileformat', 'unix')
end

local function uiSettings()
  cmd [[let $NVIM_TUI_ENABLE_TRUE_COLOR=1]]
  -- mode is shown in statusline, no need for another one
  cmd [[set noshowmode]]

  utils.opt('o', 'laststatus', 2)
  utils.opt('o', 'scrolloff', 7)      -- set 7 lines to the cursor - when moving vertically using j/k
  utils.opt('o', 'splitbelow', true)  -- split below
  utils.opt('o', 'splitright', true)  -- split on the right side

  utils.opt('w', 'number', true)          -- show current line numbers
  utils.opt('w', 'relativenumber', true)  -- enable rlative linenumbers
  utils.opt('w', 'wrap', false)
  utils.opt('w', 'cursorline', true)
  utils.opt('w', 'signcolumn', 'yes')

  utils.opt('o', 'termguicolors', true)
  utils.opt('o', 'hidden', true)
  utils.opt('o', 'showtabline', 1)
  utils.opt('o', 'updatetime', 300)
  utils.opt('o', 'showmatch', true)
  utils.opt('o', 'laststatus', 2)
  --utils.opt('o', 'completeopt', 'menuone,noinsert,noselect')
  cmd [[set completeopt=menuone,noinsert,noselect]]
  vim.o.path = vim.o.path .. '**'
end

local function backupSettings()
  cmd [[set nobackup]]
  cmd [[set nowritebackup]]

  utils.opt('o', 'undofile', true)
  utils.opt('o', 'undolevels', 1000)      -- max num of changes that can be undone
  utils.opt('o', 'undoreload', 10000)     -- max num lines to save for undo on a buffer reload
end

local function searchSettings()
  utils.opt('o', 'hlsearch', true)    -- highlight matches
  utils.opt('o', 'incsearch', true)   -- search as chars are entered
  utils.opt('o', 'ignorecase', true)  -- ignore case in searches
  utils.opt('o', 'smartcase', true)   -- unless casing in query
end

local function standardMappings()
  -- insert-mode mappings
  utils.map('i', 'jj', '<ESC>')

  -- normal-mode mappings
  utils.map('n', '<leader>hs', ':nohlsearch<cr>')
  utils.map('n', '<leader>xml', ':%!xmllint --format -<cr>')
  utils.map('n', '<leader>fo', ':copen<cr>')
  utils.map('n', '<leader>fc', ':cclose<cr>')
  utils.map('n', '<leader>fn', ':cnext<cr>')
  utils.map('n', '<leader>fp', ':cprevious<cr>')
  --utils.map('n', '<leader>nn', ':NvimTreeToggle<CR>')
  --utils.map('n', '<leader>nf', ':NvimTreeFindFile<CR>')
end

function M.setup()
  cmd [[set nocompatible]]
  cmd [[set encoding=UTF-8]]

  -- shorten time before the vim-leader-guide buffer appears
  cmd [[set timeoutlen=300]]
  cmd [[set ttimeoutlen=0]]

  -- use system clipboard
  cmd [[set clipboard=unnamed]]
  -- disable swap
  cmd [[set noswapfile]]
  -- read modelines from files
  cmd [[set modeline]]

  -- don't show line numbers in terminals
  cmd [[autocmd TermOpen * setlocal nonumber]]

  -- Leaders
  g['mapleader'] = ' '
  g['maplocalleader'] = '\\'

  -- Network read/write settings
  g['netrw_gx'] = '<cWORD>'
  g['netrw_banner'] = 3
  g['netrw_liststyle'] = 0

  tabsAndSpacesSettings()
  uiSettings()
  backupSettings()
  searchSettings()

  standardMappings()

  -------------------
  -- Uncategorized --
  -------------------

  -- make super tab strt from the top and go down
  cmd [[let g:SuperTabDefaultCompletionType = '<c-n>']]

  -- enable mouse mode
  utils.opt('o', 'mouse', 'a')

  -- set messages to be short
  utils.opt('o', 'shortmess', 'at')

  -- set soft wrap indent to be ` .`.
  utils.opt('o', 'breakindent', true)
  utils.opt('o', 'breakindentopt', 'shift:0')
  utils.opt('o', 'showbreak', '\\ \\·')

  utils.opt('o', 'wildmenu', true)
  utils.opt('o', 'wildignore', '*/tmp/*,*.so,*.swp,*.zip,*.class,*.hi,*.o,*/target/*,*/target,*/out,*/.bloop')
end

return M
