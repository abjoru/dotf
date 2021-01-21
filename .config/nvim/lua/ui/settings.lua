local fn = vim.fn
local cmd = vim.cmd
local api = require 'api'

-- Colors

-- Settings
api.set('nvim_tree_width', 40)
api.set('nvim_tree_git_hl', 1)
api.set('nvim_tree_auto_open', 1)
api.set('nvim_tree_follow', 1)
api.set('nvim_tree_hide_dotfiles', 1)

-- Tree Bindings
api.set('nvim_tree_bindings', {
  edit            = {'<CR>', 'o'},
  edit_vsplit     = '<C-v>',
  edit_split      = '<C-x>',
  edit_tab        = '<C-t>',
  close_node      = {'<S-CR>', '<BS>'},
  toggle_ignored  = 'I',
  toggle_dotfiles = 'H',
  refresh         = 'R',
  preview         = '<Tab>',
  cd              = '<C-]>',
  create          = 'a',
  remove          = 'd',
  rename          = 'r',
  cut             = 'x',
  copy            = 'c',
  paste           = 'p',
  prev_git_item   = '[c',
  next_git_item   = ']c',
  dir_up          = '-',
  close           = 'q'
})

-- Mappings
api.map('', '<F2>', ':NvimTreeToggle<CR>')
api.map('', '<C-s>', ':BufferPick<CR>')
api.map('', '<A-<>', ':BufferPrevious<CR>')
api.map('', '<A->>', ':BufferNext<CR>')
api.map('', '<A-c>', ':BufferClose<CR>')
api.map('', '<leader>', ':WhichKey \'<Space>\'<CR>')
api.map('', '?', ':WhichKey \'\'<CR>')

-- Leader Mappings
fn['which_key#register']('', {
  K = 'Show hoover',
  H = 'Toggle dotfiles in Tree',
  a = 'Create node in Tree',
  c = 'Copy node in Tree',
  d = 'Delete node in Tree'
})
