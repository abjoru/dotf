local M = {}
local g = vim.g
local cmd = vim.cmd
local map = require('dotf/utils').map
local opt = vim.opt
local globals = vim.opt_global

local indent = 2

local function tabsAndSpacesSettings()
  --cmd('set autoindent')
  --cmd('set autoread')
  --cmd('set smartindent')
  --cmd('set list')
  opt.expandtab = true
  opt.shiftwidth = indent
  opt.tabstop = indent
  opt.softtabstop = indent
  opt.fileformat = 'unix'
end

local function uiSettings()
  cmd [[let $NVIM_TUI_ENABLE_TRUE_COLOR=1]]
  -- mode is shown in statusline, no need for another one
  cmd [[set noshowmode]]

  opt.scrolloff = 7      -- set 7 lines to the cursor - when moving vertically using j/k
  opt.splitbelow = true  -- split below
  opt.splitright = true  -- split on the right side

  opt.number = true          -- show current line numbers
  opt.relativenumber = true  -- enable rlative linenumbers
  opt.wrap = false
  opt.cursorline = true
  opt.signcolumn = 'yes'

  globals.shortmess:remove("F"):append("c")
  globals.path:append("**")
  globals.termguicolors = true
  globals.hidden = true
  globals.showtabline = 1
  globals.updatetime = 300
  globals.showmatch = true
  globals.laststatus = 2
  globals.completeopt = {'menu', 'menuone', 'noselect'}

  --utils.opt('o', 'termguicolors', true)
  --utils.opt('o', 'hidden', true)
  --utils.opt('o', 'showtabline', 1)
  --utils.opt('o', 'updatetime', 300)
  --utils.opt('o', 'showmatch', true)
  --utils.opt('o', 'laststatus', 2)
  --utils.opt('o', 'completeopt', 'menuone,noinsert,noselect')
  --cmd [[set completeopt=menuone,noinsert,noselect]]
  --vim.o.path = vim.o.path .. '**'
end

local function backupSettings()
  cmd [[set nobackup]]
  cmd [[set nowritebackup]]
  cmd [[set noswapfile]]

  globals.clipboard = 'unnamed'
  globals.modeline = true
  globals.undofile = true
  globals.undolevels = 1000      -- max num of changes that can be undone
  globals.undoreload = 10000     -- max num lines to save for undo on a buffer reload
end

local function searchSettings()
  --utils.opt('o', 'hlsearch', true)    -- highlight matches
  --utils.opt('o', 'incsearch', true)   -- search as chars are entered
  globals.ignorecase = true  -- ignore case in searches
  globals.smartcase = true   -- unless casing in query
  globals.wildignore = {
    '*/tmp/*', '*.so', '*.swp', '*.zip', '*.class', '*.hi', '*.o', '*/target/*', '*/out/*', '.bloop',
    '.metals', '.ammonite'
  }
end

local function standardMappings()
  -- insert-mode mappings
  map('i', 'jj', '<ESC>')

  -- normal-mode mappings
  map('n', '<leader>hs', ':nohlsearch<cr>')
  map('n', '<leader>xml', ':%!xmllint --format -<cr>')
  map('n', '<leader>fo', ':copen<cr>')
  map('n', '<leader>fc', ':cclose<cr>')
  map('n', '<leader>fn', ':cnext<cr>')
  map('n', '<leader>fp', ':cprevious<cr>')
  map("n", "<leader>tv", ":vnew | :te<cr>")
end

function M.setup()
  cmd [[set nocompatible]]
  cmd [[set encoding=UTF-8]]

  -- shorten time before the vim-leader-guide buffer appears
  cmd [[set timeoutlen=300]]
  cmd [[set ttimeoutlen=0]]

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
  g["SuperTabDefaultCompletionType"] = "<c-n>"
  --cmd [[let g:SuperTabDefaultCompletionType = '<c-n>']]

  -- enable mouse mode
  --utils.opt('o', 'mouse', 'a')
  globals.mouse = 'a'

  -- set soft wrap indent to be ` .`.
  --utils.opt('o', 'breakindent', true)
  globals.breakindent = true
  --utils.opt('o', 'breakindentopt', 'shift:0')
  globals.breakindentopt = 'shift:0'
  --utils.opt('o', 'showbreak', '\\ \\·')
  globals.showbreak = '\\ \\·'

  --utils.opt('o', 'wildmenu', true)

end

return M
